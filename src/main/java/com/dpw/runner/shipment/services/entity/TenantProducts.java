package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.ProductType;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.util.List;

@Entity
@Getter
@Setter
@Table(name = "tenant_products")
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE tenant_products SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class TenantProducts extends MultiTenancy {

    @Enumerated(EnumType.ORDINAL)
    @Column(name = "product_type")
    private ProductType productType;

    @Column(name = "alias")
    private String alias;

    @Column(name = "priority")
    private Integer priority;

    @Column(name = "enabled")
    private Boolean enabled;

    @Column(name = "enable_grouping")
    private Boolean enableGrouping;

    @Column(name = "shipment_settings_id")
    private Long shipmentSettingsId;

    @Column(name = "is_common_sequence")
    private Boolean isCommonSequence;

    @Column(name = "transport_mode")
    @ElementCollection(targetClass = String.class, fetch = FetchType.EAGER)
    @CollectionTable(name = "tenant_products_transport_mode", joinColumns = @JoinColumn(name = "tenant_product_id"))
    @BatchSize(size = 50)
    private List<String> transportModes;
}
