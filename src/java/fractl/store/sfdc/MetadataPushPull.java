package fractl.store.sfdc;

import java.io.*;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.channels.ReadableByteChannel;
import java.rmi.RemoteException;
import java.util.*;
 
import javax.xml.parsers.*;
 
import org.w3c.dom.*;
import org.xml.sax.SAXException;
 
import com.sforce.soap.metadata.*;
 
public class MetadataPushPull {
 
    private MetadataConnection metadataConnection;
 
    private static final double API_VERSION = 29.0;
 
    // one second in milliseconds
    private static final long ASYNC_WAIT_MS = 1000;
 
    // maximum number of attempts to deploy the zip file
    private static final int MAX_NUM_POLL_REQUESTS = 50;
 
    private BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));

    public MetadataPushPull(MetadataConnection mc) {
	metadataConnection = mc;
    }

    public DeployResult deployZip(String zipFileName) throws Exception {
        byte zipBytes[] = readZipFile(zipFileName);
        DeployOptions deployOptions = new DeployOptions();
        deployOptions.setPerformRetrieve(false);
        deployOptions.setRollbackOnError(true);
        AsyncResult asyncResult = metadataConnection.deploy(zipBytes, deployOptions);
        return waitForDeployCompletion(asyncResult.getId());
    }
 
    private byte[] readZipFile(String zipFileName) throws Exception {
        byte[] result = null;
        // We assume here that you have a deploy.zip file.
        // See the retrieve sample for how to retrieve a zip file.
        File zipFile = new File(zipFileName);
        if (!zipFile.exists() || !zipFile.isFile()) {
            throw new Exception("Cannot find the zip file for deploy() on path:"
				+ zipFile.getAbsolutePath());
        }
	
        FileInputStream fileInputStream = new FileInputStream(zipFile);
        try {
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            byte[] buffer = new byte[4096];
            int bytesRead = 0;
            while (-1 != (bytesRead = fileInputStream.read(buffer))) {
                bos.write(buffer, 0, bytesRead);
            }
 
            result = bos.toByteArray();
        } finally {
            fileInputStream.close();
        }
        return result;
    }
 
    public String depoyResultAsString(DeployResult result) {
        DeployDetails details = result.getDetails();
        StringBuilder stringBuilder = new StringBuilder();
        if (details != null) {
            DeployMessage[] componentFailures = details.getComponentFailures();
            for (DeployMessage failure : componentFailures) {
                String loc = "(" + failure.getLineNumber() + ", " + failure.getColumnNumber();
                if (loc.length() == 0 && !failure.getFileName().equals(failure.getFullName()))
                {
                    loc = "(" + failure.getFullName() + ")";
                }
                stringBuilder.append(failure.getFileName() + loc + ":" 
                    + failure.getProblem()).append('\n');
            }
            RunTestsResult rtr = details.getRunTestResult();
            if (rtr.getFailures() != null) {
                for (RunTestFailure failure : rtr.getFailures()) {
                    String n = (failure.getNamespace() == null ? "" :
                        (failure.getNamespace() + ".")) + failure.getName();
                    stringBuilder.append("Test failure, method: " + n + "." +
                            failure.getMethodName() + " -- " + failure.getMessage() + 
                            " stack " + failure.getStackTrace() + "\n\n");
                }
            }
            if (rtr.getCodeCoverageWarnings() != null) {
                for (CodeCoverageWarning ccw : rtr.getCodeCoverageWarnings()) {
                    stringBuilder.append("Code coverage issue");
                    if (ccw.getName() != null) {
                        String n = (ccw.getNamespace() == null ? "" :
                        (ccw.getNamespace() + ".")) + ccw.getName();
                        stringBuilder.append(", class: " + n);
                    }
                    stringBuilder.append(" -- " + ccw.getMessage() + "\n");
                }
            }
        }
	return stringBuilder.toString();
    }
 
    public void retrieveZip(String zipFileName, String manifestFileName) throws Exception {
        RetrieveRequest retrieveRequest = new RetrieveRequest();
        // The version in package.xml overrides the version in RetrieveRequest
        retrieveRequest.setApiVersion(API_VERSION);
        setUnpackaged(retrieveRequest, manifestFileName);
 
        AsyncResult asyncResult = metadataConnection.retrieve(retrieveRequest);
        RetrieveResult result = waitForRetrieveCompletion(asyncResult);
 
        if (result.getStatus() == RetrieveStatus.Failed) {
            throw new Exception(result.getErrorStatusCode() + " msg: " +
				result.getErrorMessage());
        } else if (result.getStatus() == RetrieveStatus.Succeeded) {  
	    // Print out any warning messages
	    StringBuilder stringBuilder = new StringBuilder();
	    if (result.getMessages() != null) {
		for (RetrieveMessage rm : result.getMessages()) {
		    stringBuilder.append(rm.getFileName() + " - " + rm.getProblem() + "\n");
		}
	    }
	    if (stringBuilder.length() > 0) {
		System.out.println("Retrieve warnings:\n" + stringBuilder);
	    }
	    
	    File resultsFile = new File(zipFileName);
	    FileOutputStream os = new FileOutputStream(resultsFile);
	    
	    try {
		os.write(result.getZipFile());
	    } finally {
		os.close();
	    }
        }
    }
 
    private DeployResult waitForDeployCompletion(String asyncResultId) throws Exception {
        int poll = 0;
        long waitTimeMilliSecs = ASYNC_WAIT_MS;
        DeployResult deployResult;
        boolean fetchDetails;
        do {
            Thread.sleep(waitTimeMilliSecs);
            // double the wait time for the next iteration
 
            waitTimeMilliSecs *= 2;
            if (poll++ > MAX_NUM_POLL_REQUESTS) {
                throw new Exception(
                    "Request timed out. If this is a large set of metadata components, " +
                    "ensure that MAX_NUM_POLL_REQUESTS is sufficient.");
            }
            // Fetch in-progress details once for every 3 polls
            fetchDetails = (poll % 3 == 0);
 
            deployResult = metadataConnection.checkDeployStatus(asyncResultId, fetchDetails);
        }
        while (!deployResult.isDone());
	
        if (!deployResult.isSuccess() && deployResult.getErrorStatusCode() != null) {
            throw new Exception(deployResult.getErrorStatusCode() + " msg: " +
				deployResult.getErrorMessage());
        }
        
        if (!fetchDetails) {
            // Get the final result with details if we didn't do it in the last attempt.
            deployResult = metadataConnection.checkDeployStatus(asyncResultId, true);
        }

        return deployResult;
    }
 
    private RetrieveResult waitForRetrieveCompletion(AsyncResult asyncResult) throws Exception {
    	// Wait for the retrieve to complete
        int poll = 0;
        long waitTimeMilliSecs = ASYNC_WAIT_MS;
        String asyncResultId = asyncResult.getId();
        RetrieveResult result = null;
        do {
            Thread.sleep(waitTimeMilliSecs);
            // Double the wait time for the next iteration
            waitTimeMilliSecs *= 2;
            if (poll++ > MAX_NUM_POLL_REQUESTS) {
                throw new Exception("Request timed out.  If this is a large set " +
                "of metadata components, check that the time allowed " +
                "by MAX_NUM_POLL_REQUESTS is sufficient.");
            }
            result = metadataConnection.checkRetrieveStatus(
                    asyncResultId, true);
        } while (!result.isDone());         
 
        return result;
    }
 
    private void setUnpackaged(RetrieveRequest request, String manifestFileName) throws Exception {
        // Edit the path, if necessary, if your package.xml file is located elsewhere
        File unpackedManifest = new File(manifestFileName);

        if (!unpackedManifest.exists() || !unpackedManifest.isFile()) {
            throw new Exception("Should provide a valid retrieve manifest " +
				"for unpackaged content. Looking for " +
				unpackedManifest.getAbsolutePath());
        }
 
        // Note that we use the fully quualified class name because
        // of a collision with the java.lang.Package class
        com.sforce.soap.metadata.Package p = parsePackageManifest(unpackedManifest);
        request.setUnpackaged(p);
    }
 
    private com.sforce.soap.metadata.Package parsePackageManifest(File file)
	throws ParserConfigurationException, IOException, SAXException {
        com.sforce.soap.metadata.Package packageManifest = null;
        List<PackageTypeMembers> listPackageTypes = new ArrayList<PackageTypeMembers>();
        DocumentBuilder db =
	    DocumentBuilderFactory.newInstance().newDocumentBuilder();
        InputStream inputStream = new FileInputStream(file);
        Element d = db.parse(inputStream).getDocumentElement();
        for (Node c = d.getFirstChild(); c != null; c = c.getNextSibling()) {
            if (c instanceof Element) {
                Element ce = (Element) c;
                NodeList nodeList = ce.getElementsByTagName("name");
                if (nodeList.getLength() == 0) {
                    continue;
                }
                String name = nodeList.item(0).getTextContent();
                NodeList m = ce.getElementsByTagName("members");
                List<String> members = new ArrayList<String>();
                for (int i = 0; i < m.getLength(); i++) {
                    Node mm = m.item(i);
                    members.add(mm.getTextContent());
                }
                PackageTypeMembers packageTypes = new PackageTypeMembers();
                packageTypes.setName(name);
                packageTypes.setMembers(members.toArray(new String[members.size()]));
                listPackageTypes.add(packageTypes);
            }
        }
        packageManifest = new com.sforce.soap.metadata.Package();
        PackageTypeMembers[] packageTypesArray =
	    new PackageTypeMembers[listPackageTypes.size()];
        packageManifest.setTypes(listPackageTypes.toArray(packageTypesArray));
        packageManifest.setVersion(API_VERSION + "");
        return packageManifest;
    }
}
