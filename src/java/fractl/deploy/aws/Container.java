package fractl.deploy.aws;

import java.util.concurrent.CompletableFuture;
import clojure.lang.IFn;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.ecr.EcrAsyncClient;
import software.amazon.awssdk.services.ecr.model.CreateRepositoryRequest;
import software.amazon.awssdk.services.ecr.model.CreateRepositoryResponse;

public final class Container {
    public static EcrAsyncClient buildClient(Region region) {
	return EcrAsyncClient.builder()
	    .region(region)
	    .build();
    }

    public static void createRepository(EcrAsyncClient client, String reponame, IFn callback) {
	CreateRepositoryRequest req = CreateRepositoryRequest.builder()
	    .repositoryName(reponame)
	    .build();
	CompletableFuture<CreateRepositoryResponse> future = client.createRepository(req);
	future.whenComplete((reponse, err) -> {
		callback.invoke(reponse, err);
	    });
    }
}
