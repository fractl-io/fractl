package fractl.filesystem;

import java.io.*;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.zip.*;
import java.util.HashMap;
import java.util.Map;
import java.net.URI;

public class Zip {

    public static String zipFolder(String path) throws IOException {

        Path source = Paths.get(path);
	
        if (!Files.isDirectory(source))
            throw new IOException("Source is not a folder.");

        String currentPath = System.getProperty("user.dir") + File.separator;
	
        String zipFileName = source.getFileName().toString() + ".zip";
        URI uri = URI.create("jar:file:" + currentPath + zipFileName);

        Files.walkFileTree(source, new SimpleFileVisitor<>() {
		@Override
		public FileVisitResult visitFile(Path file,
						 BasicFileAttributes attributes) {

		    // Copying of symbolic links not supported
		    if (attributes.isSymbolicLink()) {
			return FileVisitResult.CONTINUE;
		    }

		    Map<String, String> env = new HashMap<>();
		    env.put("create", "true");

		    try (FileSystem zipfs = FileSystems.newFileSystem(uri, env)) {

			Path targetFile = source.relativize(file);
			Path pathInZipfile = zipfs.getPath(targetFile.toString());
			// NoSuchFileException, need create parent directories in zip path
			if (pathInZipfile.getParent() != null) {
			    Files.createDirectories(pathInZipfile.getParent());
			}

			// copy file attributes
			CopyOption[] options = {
                            StandardCopyOption.REPLACE_EXISTING,
                            StandardCopyOption.COPY_ATTRIBUTES,
                            LinkOption.NOFOLLOW_LINKS
			};
			// Copy a file into the zip file path
			Files.copy(file, pathInZipfile, options);

		    } catch (IOException e) {
			e.printStackTrace();
		    }
		
		    return FileVisitResult.CONTINUE;
		}
		
		@Override
		public FileVisitResult visitFileFailed(Path file, IOException exc) {
		    System.err.printf("Unable to zip : %s%n%s%n", file, exc);
		    return FileVisitResult.CONTINUE;
		}
	    });
	return zipFileName;
    }

    private static File newFile(File destinationDir, ZipEntry zipEntry) throws IOException {
	File destFile = new File(destinationDir, zipEntry.getName());
	String destDirPath = destinationDir.getCanonicalPath();
	String destFilePath = destFile.getCanonicalPath();

	if (!destFilePath.startsWith(destDirPath + File.separator)) {
	    throw new IOException("Entry is outside of the target dir: " + zipEntry.getName());
	}

	return destFile;
    }

    public static void unzip(String zipFileName, String destDirName) throws IOException {
        File destDir = new File(destDirName);
        byte[] buffer = new byte[1024];
        ZipInputStream zis = new ZipInputStream(new FileInputStream(zipFileName));
        ZipEntry zipEntry = zis.getNextEntry();
        while (zipEntry != null) {
	    File newFile = newFile(destDir, zipEntry);
	    System.out.println(newFile + ", " + zipEntry);
	    if (zipEntry.isDirectory()) {
		if (!newFile.isDirectory() && !newFile.mkdirs()) {
		    throw new IOException("Failed to create directory " + newFile);
		}
	    } else {
		// fix for Windows-created archives
		File parent = newFile.getParentFile();
		if (!parent.isDirectory() && !parent.mkdirs()) {
		    throw new IOException("Failed to create directory " + parent);
		}

		// write file content
		FileOutputStream fos = new FileOutputStream(newFile);
		int len;
		while ((len = zis.read(buffer)) > 0) {
		    fos.write(buffer, 0, len);
		}
		fos.close();
	    }
	    zipEntry = zis.getNextEntry();
        }
        zis.closeEntry();
        zis.close();
    }
}
