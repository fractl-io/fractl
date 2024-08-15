package agentlang.subs;

import java.util.Map;
import java.util.HashMap;
import java.util.Collection;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.clients.consumer.ConsumerRebalanceListener;
import org.apache.kafka.clients.consumer.OffsetAndMetadata;
import org.apache.kafka.common.TopicPartition;

public class HandleRebalance implements ConsumerRebalanceListener {
    private Map<TopicPartition, OffsetAndMetadata> currentOffsets = new HashMap<>();
    private KafkaConsumer consumer;

    public HandleRebalance(KafkaConsumer consumer) {
	this.consumer = consumer;
    }

    public void addOffset(ConsumerRecord record) {
	currentOffsets.put(new TopicPartition(record.topic(), record.partition()),
			   new OffsetAndMetadata(record.offset()+1, null));
    }

    public Map<TopicPartition, OffsetAndMetadata> getCurrentOffsets() {
	return currentOffsets;
    }

    public void resetCurrentOffsets() {
	currentOffsets.clear();
    }

    public void onPartitionsAssigned(Collection<TopicPartition>
        partitions) {
    }

    public void onPartitionsRevoked(Collection<TopicPartition> partitions) {
        consumer.commitSync(currentOffsets);
	currentOffsets.clear();
    }
}
