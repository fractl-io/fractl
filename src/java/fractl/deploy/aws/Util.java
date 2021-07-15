package fractl.deploy.aws;

import software.amazon.awssdk.regions.Region;

public final class Util {
    public static Region regionFromName(String regionName) {
	return Region.of(regionName);
    }
}
