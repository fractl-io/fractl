package fractl.filesystem;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.HashMap;
import java.util.Map;

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
}
