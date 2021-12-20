package fractl.auth.auth0;

import com.auth0.client.auth.AuthAPI;
import com.auth0.client.auth.AuthorizeUrlBuilder;
import com.auth0.json.auth.TokenHolder;
import com.auth0.json.auth.CreatedUser;
import com.auth0.exception.Auth0Exception;

public class Auth0AuthUtil {

	private static final String _dbConnection = "Username-Password-Authentication";
		
	private static AuthAPI createAuthAPI(String clientId, String clientSecret, String authDomain) {
        AuthAPI auth = new AuthAPI(authDomain, clientId, clientSecret);
        auth.setLoggingEnabled(true);

		return auth;
	}
	
    public static String authorizeUrl(String clientId, String clientSecret, String authDomain,
                                      String authCallbackUrl, String scope) {
        
		AuthAPI auth = createAuthAPI(clientId, clientSecret, authDomain);
        String authUrl = auth.authorizeUrl(authCallbackUrl).withScope(scope).build();

        return authUrl;
    }

	public static TokenHolder PasswordLogin(String clientId, String clientSecret, String authDomain,
											String userNameOrEmail, String password, String scope) {

		AuthAPI auth = createAuthAPI(clientId, clientSecret, authDomain);
		
		try {
			TokenHolder result = auth.login(userNameOrEmail, password.toCharArray()).setScope(scope).execute();
			return result;
		} catch (Auth0Exception ex) {
			ex.printStackTrace();
			return null;
		}
	}

	public static CreatedUser SignupUser(String clientId, String clientSecret, String authDomain,
										 String userName, String userEmail, String password) {

		AuthAPI auth = createAuthAPI(clientId, clientSecret, authDomain);
		
		try {
			CreatedUser user = auth.signUp(userEmail, userName, password.toCharArray(), _dbConnection).execute();
			return user;
		} catch (Auth0Exception ex) {
			ex.printStackTrace();
			return null;
		}		

	}

}
