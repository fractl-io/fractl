package agentlang.filesystem;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.regex.*;

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
	if (directoryToBeDeleted.exists()) {
	    File[] allContents = directoryToBeDeleted.listFiles();
	    if (allContents != null) {
		for (File file : allContents) {
		    _forceDeleteDirectory(file);
		}
	    }
	    return directoryToBeDeleted.delete();
	} else return true;
    }

    public static void copyDirectory(String src, String dest)
	throws IOException {
	Files.walk(Paths.get(src))
	    .forEach(source -> {
		    Path destination =
			Paths.get(dest, source.toString().substring(src.length()));
		    try {
			Files.copy(source, destination);
		    } catch (IOException e) {
			e.printStackTrace();
		    }
		});
    }

    public static boolean deleteFile(String fileName) {
	File f = new File(fileName);
	return f.delete();
    }

    public static File[] listFilesByExtn(String dir, String extn) {
	File directoryPath = new File(dir);
	FilenameFilter ff = new FilenameFilter(){
		public boolean accept(File dir, String name) {
		    String lowercaseName = name.toLowerCase();
		    if (lowercaseName.endsWith(extn)) {
			return true;
		    } else {
			return false;
		    }
		}
	    };
	return directoryPath.listFiles(ff);
    }

    private static Pattern wildcardToRegex(String wildcard){
        StringBuffer s = new StringBuffer(wildcard.length());
        s.append('^');
        for (int i = 0, is = wildcard.length(); i < is; i++) {
            char c = wildcard.charAt(i);
            switch(c) {
	    case '*':
		s.append(".*");
		break;
	    case '?':
		s.append(".");
		break;
		// escape special regexp-characters
	    case '(': case ')': case '[': case ']': case '$':
	    case '^': case '.': case '{': case '}': case '|':
	    case '\\':
		s.append("\\");
		s.append(c);
		break;
	    default:
		s.append(c);
		break;
            }
        }
        s.append('$');
        return Pattern.compile(s.toString());
    }

    public static File[] listFilesByName(String dir, String wildcard) {
	File directoryPath = new File(dir);
	Pattern pattern = wildcardToRegex(wildcard);
	FilenameFilter ff = new FilenameFilter() {
		public boolean accept(File dir, String name) {
		    Matcher m = pattern.matcher(name);
		    return m.matches();
		}
	    };
	return directoryPath.listFiles(ff);
    }
}
