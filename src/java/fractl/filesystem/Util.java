package fractl.filesystem;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;

public final class Util {

    public static void maybeCreateDirectories(String path)
	throws IOException {
	File folder = new File(path);
	if (!folder.exists())
	    Files.createDirectories(folder.toPath());
    }

    public static void copyOrReplaceFile(String src, String dest)
	throws IOException {
	File a = new File(src);
	File b = new File(dest);
        Files.copy (a.toPath(), b.toPath(), StandardCopyOption.REPLACE_EXISTING);
    }

    public static boolean forceDeleteDirectory(String dirName) {
	return _forceDeleteDirectory(new File(dirName));
    }

    private static boolean _forceDeleteDirectory(File directoryToBeDeleted) {
	File[] allContents = directoryToBeDeleted.listFiles();
	if (allContents != null) {
	    for (File file : allContents) {
		_forceDeleteDirectory(file);
	    }
	}
	return directoryToBeDeleted.delete();
    }

    public static boolean deleteFile(String fileName) {
	File f = new File(fileName);
	return f.delete();
    }
}
