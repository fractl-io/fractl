package fractl.aws;

import com.amazonaws.services.lambda.runtime.RequestHandler;
import com.amazonaws.services.lambda.runtime.Context;

public class LambdaHandler implements RequestHandler<Void, String> {
 
  @Override
  public String handleRequest(Void input, Context context) {
      return "Hello World from Fractl!";
  }
}
