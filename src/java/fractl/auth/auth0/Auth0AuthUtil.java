package fractl.auth.auth0;

import com.auth0.client.auth.AuthAPI;
import com.auth0.client.mgmt.ManagementAPI;
import com.auth0.client.auth.AuthorizeUrlBuilder;
import com.auth0.json.auth.TokenHolder;
import com.auth0.json.auth.CreatedUser;
import com.auth0.exception.Auth0Exception;
import com.auth0.exception.APIException;
import com.auth0.json.auth.UserInfo;
import java.util.Map;

public class Auth0AuthUtil {

	private static final String _dbConnection = "Username-Password-Authentication";
		
	public static AuthAPI createAuthAPI(String clientId, String clientSecret, String authDomain, boolean loggingEnabled) {
        AuthAPI api = new AuthAPI(authDomain, clientId, clientSecret);
        api.setLoggingEnabled(loggingEnabled);

		return api;
	}

	// This requires a management API authtoken - right now we have to set this as part of the
	// _requestobject of the kernel__auth0user record in the database in the field "ApiToken"
	// The token can be obtained from
	// https://manage.auth0.com/dashboard/undefined/fractl/apis/management/explorer
	public static ManagementAPI createMgmtAPI(String authDomain, String accessToken, boolean loggingEnabled) {
		ManagementAPI api = new ManagementAPI(authDomain, accessToken);
		api.setLoggingEnabled(loggingEnabled);

		return api;
	}
	
    public static String authorizeUrl(AuthAPI api, String authCallbackUrl, String scope) {
        return api.authorizeUrl(authCallbackUrl).withScope(scope).build();
    }

	public static TokenHolder passwordLogin(AuthAPI api, String userNameOrEmail, String password, String scope) {

		try {
			TokenHolder result = api.login(userNameOrEmail, password.toCharArray()).setScope(scope).execute();
			return result;
		} catch (Auth0Exception ex) {
			ex.printStackTrace();
			return null;
		}
	}

	public static CreatedUser signupUser(AuthAPI api, String userName, String userEmail, String password,
										 Map<String, String> customFields) {

		try {
			CreatedUser user = api.signUp(userEmail, userName, password.toCharArray(), _dbConnection).setCustomFields(customFields).execute();
			return user;
		} catch (Auth0Exception ex) {
			ex.printStackTrace();
			return null;
		}		

	}

	public static Map<String, Object> getUserInfo(AuthAPI api, String accessToken) {
		try {
			return api.userInfo(accessToken).execute().getValues();
		} catch (Auth0Exception ex) {
			ex.printStackTrace();
			return null;
		}		
	}

	public static Map<String, Object> getUserInfo(AuthAPI api, TokenHolder tokenHolder) {
		try {
			return api.userInfo(tokenHolder.getAccessToken()).execute().getValues();
		} catch (Auth0Exception ex) {
			ex.printStackTrace();
			return null;
		}		
	}

	public static Void deleteUser(ManagementAPI api, String userId) {
		try {
			return api.users().delete(userId).execute();
		} catch (APIException ex) {
			ex.printStackTrace();
			return null;			
		} catch (Auth0Exception ex) {
			ex.printStackTrace();
			return null;
		}					
	}

}
