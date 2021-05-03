package fractl.store.sfdc;

import com.sforce.soap.enterprise.EnterpriseConnection;
import com.sforce.soap.enterprise.LoginResult;
import com.sforce.soap.metadata.MetadataConnection;
import com.sforce.ws.ConnectionException;
import com.sforce.ws.ConnectorConfig;
 
public class MetadataLoginUtil {
 
    public static MetadataConnection login(String username, String password, String url) throws ConnectionException {
        final LoginResult loginResult = loginToSalesforce(username, password, url);
        return createMetadataConnection(loginResult);
    }
 
    private static MetadataConnection createMetadataConnection(
            final LoginResult loginResult) throws ConnectionException {
        final ConnectorConfig config = new ConnectorConfig();
        config.setServiceEndpoint(loginResult.getMetadataServerUrl());
        config.setSessionId(loginResult.getSessionId());
        return new MetadataConnection(config);
    }
 
    private static LoginResult loginToSalesforce(
            final String username,
            final String password,
            final String loginUrl) throws ConnectionException {
        final ConnectorConfig config = new ConnectorConfig();
        config.setAuthEndpoint(loginUrl);
        config.setServiceEndpoint(loginUrl);
        config.setManualLogin(true);
	try {
	    return (new EnterpriseConnection(config)).login(username, password);
	} catch(Exception ex) {
	    ex.printStackTrace();
	    return null;
	}
    }
}
