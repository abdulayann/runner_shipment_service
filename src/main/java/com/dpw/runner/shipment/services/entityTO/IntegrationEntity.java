package com.dpw.runner.shipment.services.entityTO;

import com.dpw.runner.shipment.services.entity.TOBaseEntity;
import com.dpw.runner.shipment.services.entity.enums.MessageType;
import com.dpw.runner.shipment.services.entity.enums.StatusType;
import com.dpw.runner.shipment.services.utils.HashMapConverter;
import com.dpw.runner.shipment.services.utils.annotation.SQLRestriction;
import lombok.*;
import lombok.extern.slf4j.Slf4j;

import javax.persistence.*;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Setter
@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity(name = "integration_payload")
@Slf4j
@Table
@SQLRestriction("isActive = true")
public class IntegrationEntity extends TOBaseEntity {


    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(columnDefinition = "uuid", updatable = false, nullable = false, unique = true, insertable = false)
    private UUID id;



    @Column
    @Enumerated(EnumType.STRING)
    private MessageType messageType;



    @Column
    private String entityId;

    @Column
    private String entityType;

    @Column
    private String awbNumber;

    @Convert(converter = HashMapConverter.class)
    @Column(columnDefinition = "text")
    private Map<String, Object> inPayload;

    @Convert(converter = HashMapConverter.class)
    @Column(columnDefinition = "text")
    private Map<String, Object> outPayload;

    @OneToMany(mappedBy = "integrationEntityId")
    private List<ResponseEntity> statusList;

    @Column
    @Enumerated(EnumType.STRING)
    private StatusType status;

    @Column(columnDefinition = "text")
    private String metaData;

    @Column
    private String referenceNumber;

    @Column
    private  String uniqueId;

    @Column(columnDefinition = "text")
    private String xmlData;

}
