package fractl.deploy.aws;

import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.ecr.EcrAsyncClient;

public final class Container {
    public static EcrAsyncClient buildClient(Region region) {
	EcrAsyncClient client = EcrAsyncClient.builder()
	    .region(region)
	    .build();
	return client;
    }
}
