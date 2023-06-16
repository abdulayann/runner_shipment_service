package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.filter.Multitenancy.MultiTenancy;
import com.vladmihalcea.hibernate.type.json.JsonType;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.TypeDef;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Setter
@Getter
@Table(name = "carrier_details")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@TypeDef(name = "jsonb", typeClass = JsonType.class)
public class CarrierDetails extends MultiTenancy {

    private static final long serialVersionUID = 190794279984274725L;

    @Id
    @ToString.Include
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "guid")
    private UUID guid;

    @Column(name = "shipping_line")
    private String shippingLine;

    @Column(name = "vessel")
    private String vessel;

    @Column(name = "voyage")
    private String voyage;

    @Column(name = "origin")
    private String origin;

    @Column(name = "destination")
    private String destination;

    @Column(name = "eta")
    private LocalDateTime eta;

    @Column(name = "etd")
    private LocalDateTime etd;

    @Column(name = "ata")
    private LocalDateTime ata;

    @Column(name = "atd")
    private LocalDateTime atd;
}
