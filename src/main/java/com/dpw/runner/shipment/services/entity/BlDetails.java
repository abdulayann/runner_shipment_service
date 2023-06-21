package com.dpw.runner.shipment.services.entity;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.TypeDef;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;


@Entity
@Setter
@Getter
@Table(name = "bl_details")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class)
public class BlDetails extends MultiTenancy {

    private static final long serialVersionUID = 190794279984274725L;

    @Column(name = "guid")
    private UUID guid;

    @Column(name = "release_type")
    private String releaseType;

    @Column(name = "house_bill_type")
    private String hblType;

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Column(name = "delivery_mode")
    private String deliveryMode;

    @Column(name = "screening_status")
    private String screeningStatus;

    @Column(name = "paid_place")
    private Long paidPlace;

    @Column(name = "place_of_issue")
    private Long placeOfIssue;

    @Column(name = "date_of_issue")
    private LocalDateTime dateOfIssue;

    @Column(name = "date_of_receipt")
    private LocalDateTime dateOfReceipt;

    @Column(name = "goods_co")
    private String goodsCo;

    @Column(name = "boe_date")
    private LocalDateTime boeDate;

    @Column(name = "boe_number")
    private String boeNumber;
}
