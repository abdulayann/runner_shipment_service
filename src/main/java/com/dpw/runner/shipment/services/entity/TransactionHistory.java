package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.FlowType;
import com.dpw.runner.shipment.services.entity.enums.SourceSystem;
import com.dpw.runner.shipment.services.entity.enums.VerifiedGrossMassStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;
import java.time.LocalDateTime;

@Entity
@Table(name = "transaction_history")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TransactionHistory extends MultiTenancy {

    @Enumerated(EnumType.STRING)
    @Column(name = "action_status")
    private VerifiedGrossMassStatus actionStatus;

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

    @Column(name = "verified_gross_mass_id")
    private Long verifiedGrossMassId;
}
