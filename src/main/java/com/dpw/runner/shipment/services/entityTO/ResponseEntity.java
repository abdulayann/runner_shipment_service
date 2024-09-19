package com.dpw.runner.shipment.services.entityTO;

import com.dpw.runner.shipment.services.entity.TOBaseEntity;
import com.dpw.runner.shipment.services.entity.enums.ResponseMessageType;
import com.dpw.runner.shipment.services.utils.HashMapConverter;
import com.dpw.runner.shipment.services.utils.annotation.SQLRestriction;
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
@Entity(name = "ack_payload")
@Slf4j
@Table
@SQLRestriction("isActive = true")
public class ResponseEntity extends TOBaseEntity {



    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(columnDefinition = "uuid", updatable = false, nullable = false, unique = true, insertable = false)
    private UUID id;


    @ManyToOne
    @JoinColumn(name = "integration_payload_table_id")
    private IntegrationEntity integrationEntityId;

    @Column
    @Enumerated(EnumType.STRING)
    private ResponseMessageType messageTypeKey;

    @Column(columnDefinition = "text")
    private String messageTypeValue;

    @Column
    private String transactionId;

    @Column(columnDefinition = "text")
    private String metaData;

    @Convert(converter = HashMapConverter.class)
    @Column(columnDefinition = "text")
    private Map<String, Object> inPayload;

    @Convert(converter = HashMapConverter.class)
    @Column(columnDefinition = "text")
    private Map<String, Object> outPayload;

    @Column(columnDefinition = "text")
    private String xmlData;

}
