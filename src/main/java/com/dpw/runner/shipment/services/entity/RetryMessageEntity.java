package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.enums.EventMessageStatusEnum;
import com.dpw.runner.shipment.services.utils.HashMapConverter;
import lombok.*;
import lombok.extern.slf4j.Slf4j;

import javax.persistence.*;
import java.util.Map;
import java.util.UUID;

@Setter
@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity(name = "retry_message")
@Slf4j
@Table
public class RetryMessageEntity extends TOBaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(columnDefinition = "uuid", updatable = false, nullable = false, unique = true, insertable = false)
    private UUID id;

    @Column(columnDefinition = "text")
    private String payload;

    private String topic;

    @Enumerated(EnumType.STRING)
    private com.dpw.runner.shipment.services.entity.enums.MessageTypeEnum messageType;

    @Convert(converter = HashMapConverter.class)
    @Column(columnDefinition = "text")
    private Map<String, String> headers;

    private Integer retryCount;

    @Enumerated(EnumType.STRING)
    private EventMessageStatusEnum status;

    private String messageKey;
}
