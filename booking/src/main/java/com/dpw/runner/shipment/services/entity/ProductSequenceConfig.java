package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.GenerationType;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;
import javax.persistence.*;
import java.time.LocalDateTime;

@Entity
@Getter
@Setter
@Table(name = "product_sequence_config")
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE product_sequence_config SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class ProductSequenceConfig extends MultiTenancy {

    @OneToOne(targetEntity = TenantProducts.class)
    @JoinColumn(name = "tenant_products_id", referencedColumnName = "id")
    private TenantProducts tenantProducts;

    @Enumerated(EnumType.ORDINAL)
    @Column(name = "product_process_types")
    private ProductProcessTypes productProcessTypes;

    @Column(name = "sequence_group")
    private String sequenceGroup;

    @Column(name = "sequence_group_for_payment_no_gen")
    private String sequenceGroupForPaymentNoGen;

    @Column(name = "generation_type")
    private GenerationType generationType;

    @Column(name = "prefix")
    private String prefix;

    @Column(name = "serial_counter")
    private Integer serialCounter;

    @Column(name = "sequence_start_time")
    private LocalDateTime sequenceStartTime;

    @Column(name = "shipment_settings_id")
    private Long shipmentSettingsId;
}
