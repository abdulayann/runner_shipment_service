package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.TypeDef;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.util.UUID;

@Entity
@Setter
@Getter
@Table(name = "air_messaging_logs")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Accessors(chain = true)
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class)
@JsonIgnoreProperties(ignoreUnknown = true)
public class AirMessagingLogs extends MultiTenancy {
    @Column(name = "entity_guid")
    private UUID entityGuid;

    @Column(name = "error_message")
    private String errorMessage;

    @Column(name = "message_type")
    private String messageType;

    @Column(name = "xml_payload")
    private String xmlPayload;

    @Column(name = "status")
    private String status;
}
