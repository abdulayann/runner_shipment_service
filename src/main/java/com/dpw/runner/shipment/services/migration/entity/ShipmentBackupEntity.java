package com.dpw.runner.shipment.services.migration.entity;

import com.dpw.runner.shipment.services.dto.response.PickupDeliveryDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import com.vladmihalcea.hibernate.type.json.JsonType;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import java.time.Instant;
import java.util.List;
import java.util.UUID;

@Entity
@Setter
@Getter
@Table(name = "shipment_backup")
@NoArgsConstructor
@AllArgsConstructor
public class ShipmentBackupEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private Integer tenantId;

    @Column(name = "back_up_time_utc", nullable = false)
    private Instant backupTimeUtc = Instant.now();

    @Column(name = "shipment_id")
    private String shipmentId;

    @Column(name = "shipment_guid")
    private UUID shipmentGuid;

    @Type(type = "jsonb")
    @Column(name = "shipment_details" , columnDefinition = "jsonb")
    private ShipmentDetailsResponse shipmentDetail;

    @Type(type = "jsonb")
    @Column(name  = "pickup_delivery_detail",  columnDefinition = "jsonb")
    private List<PickupDeliveryDetailsResponse> pickupDeliveryDetail;
}
