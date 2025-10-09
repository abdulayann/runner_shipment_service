package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.EntityTypeTransactionHistory;
import com.dpw.runner.shipment.services.entity.enums.FlowType;
import com.dpw.runner.shipment.services.entity.enums.SourceSystem;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;
import java.time.LocalDateTime;

@Entity
@Table(name = "transaction_history")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TransactionHistory extends MultiTenancy {

    @Column(name = "action_status")
    private String actionStatusDescription;

    @Enumerated(EnumType.STRING)
    @Column(name = "flow_type")
    private FlowType flowType;

    @Column(name = "description")
    private String description;

    @Enumerated(EnumType.STRING)
    @Column(name = "source_system")
    private SourceSystem sourceSystem;

    @Column(name = "actual_date_time")
    private LocalDateTime actualDateTime;

    @Column(name = "error_message")
    private String errorMessage;

    @Column(name = "entity_type", length = 50)
    @Enumerated(value = EnumType.STRING)
    private EntityTypeTransactionHistory entityType;

    @Column(name = "entity_id")
    private Long entityId;
}
