package fractl.aws;

import java.util.concurrent.atomic.AtomicReference;
import com.amazonaws.services.lambda.runtime.RequestHandler;
import com.amazonaws.services.lambda.runtime.Context;
import clojure.lang.IFn;
import clojure.lang.ISeq;

public class LambdaHandler implements RequestHandler<Object, String> {

    static private IFn callback;
    static private AtomicReference evaluator = new AtomicReference();

    @Override
    public String handleRequest(Object input, Context context) {
	IFn result = (IFn)callback.invoke(evaluator.get(), input);
	if (evaluator.get() == null) {
	    evaluator.set(result.invoke(1));
	}
	return result.invoke(0).toString();
    }

    public static void setCallback(IFn fn) {
	callback = fn;
    }
}
