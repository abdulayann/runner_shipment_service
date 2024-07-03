package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.DateType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.time.LocalDateTime;

@Entity
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "date_time_change_logs")
@SQLDelete(sql = "UPDATE date_time_change_logs SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class DateTimeChangeLog extends MultiTenancy {

    @Column(name = "current_value")
    LocalDateTime currentValue;

    @Enumerated(EnumType.ORDINAL)
    @Column(name = "date_type")
    DateType dateType;

    @Column(name = "shipment_id")
    Long shipmentId;

    @Column(name = "source_of_update")
    String sourceOfUpdate;
}
