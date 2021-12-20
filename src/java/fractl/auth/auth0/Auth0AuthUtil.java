package fractl.auth.auth0;

import com.auth0.client.auth.AuthAPI;
import com.auth0.client.auth.AuthorizeUrlBuilder;

public class Auth0AuthUtil {

    public static String authorizeUrl(String clientId, String clientSecret, String authDomain,
                                      String authCallbackUrl, String scope) {
        
        AuthAPI auth = new AuthAPI(authDomain, clientId, clientSecret);
        auth.setLoggingEnabled(true);

        String authUrl = auth.authorizeUrl(authCallbackUrl).withScope(scope).build();

        return authUrl;
    }

}
