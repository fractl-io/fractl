package fractl.auth.auth0;

import com.auth0.client.auth.AuthAPI;
import com.auth0.client.auth.AuthorizeUrlBuilder;
import com.auth0.json.auth.TokenHolder;
import com.auth0.json.auth.CreatedUser;
import com.auth0.exception.Auth0Exception;
import com.auth0.json.auth.UserInfo;
import java.util.Map;

// For entity reference - remove later
  // (entity {:Kernel/Authentication
  //          {:Owner :Kernel/Any
  //           :AuthType {:oneof [:Database :OAuth2Request :Auth0Database]}
  //           :RequestObject :Kernel/Map ;; key-value pair 
  //           :Issued {:type :Kernel/DateTime :optional true}
  //           :ExpirySeconds {:type :Kernel/Int :default 300}}})

  // (record {:Kernel/AuthResponse
  //          {:AuthToken :Kernel/String
  //           :IdToken :Kernel/String
  //           :RefreshToken :Kernel/String
  //           }})
  

public class Auth0AuthUtil {

	private static final String _dbConnection = "Username-Password-Authentication";
		
	public static AuthAPI createAuthAPI(String clientId, String clientSecret, String authDomain) {
        AuthAPI api = new AuthAPI(authDomain, clientId, clientSecret);
        api.setLoggingEnabled(true);

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

	public static UserInfo getUserInfo(AuthAPI api, String accessToken) {
		try {
			return api.userInfo(accessToken).execute();
		} catch (Auth0Exception ex) {
			ex.printStackTrace();
			return null;
		}		
	}

	public static UserInfo getUserInfo(AuthAPI api, TokenHolder tokenHolder) {
		try {
			return api.userInfo(tokenHolder.getAccessToken()).execute();
		} catch (Auth0Exception ex) {
			ex.printStackTrace();
			return null;
		}		
	}	

}
