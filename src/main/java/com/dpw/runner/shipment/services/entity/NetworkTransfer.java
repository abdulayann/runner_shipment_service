package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferSource;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.TenantIdData;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;

import javax.persistence.*;
import javax.validation.constraints.Size;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.UUID;

@Entity
@Setter
@Getter
@Table(name = "network_transfer")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class)
public class NetworkTransfer extends MultiTenancy {

    @Column(name = "entity_type")
    @Size(max = 255, message = "max size is 255 for entityType")
    private String entityType;

    @Column(name = "entity_number")
    @Size(max = 255, message = "max size is 255 for entityNumber")
    private String entityNumber;

    @Column(name = "entity_id")
    private Long entityId;

    @Column(name = "entity_guid", updatable = false)
    private UUID entityGuid;

    @Column(name = "is_hidden")
    private Boolean isHidden = false;

    @Column(name = "created_entity_id")
    private Long createdEntityId;

    @Column(name = "transport_mode")
    @Size(max = 255, message = "max size is 255 for transportMode")
    @MasterData(type = MasterDataType.TRANSPORT_MODE)
    private String transportMode;

    @Column(name = "source_branch_id")
    @TenantIdData
    private Integer sourceBranchId;

    @Column(name = "status")
    @Enumerated(EnumType.STRING)
    private NetworkTransferStatus status;

    @Column(name = "job_type")
    @Size(max = 255, message = "max size is 255 for jobType")
    @MasterData(type = MasterDataType.CUSTOM_SHIPMENT_TYPE)
    private String jobType;

    @Type(type = "jsonb")
    @Column(name = "entity_payload", columnDefinition = "jsonb")
    @SuppressWarnings("java:S1948")
    private Map<String, Object> entityPayload;

    @Column(name = "is_inter_branch_entity")
    private Boolean isInterBranchEntity = false;

    @Column(name = "transferred_date")
    private LocalDateTime transferredDate;

    @Column(name = "source")
    @Enumerated(EnumType.STRING)
    private NetworkTransferSource source;

}
