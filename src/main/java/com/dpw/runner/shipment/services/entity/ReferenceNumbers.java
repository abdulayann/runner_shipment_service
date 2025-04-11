package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;

@Entity
@Setter
@Getter
@Table(name = "reference_numbers")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE reference_numbers SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class ReferenceNumbers extends MultiTenancy {

    @Column(name = "consolidation_id")
    private Long consolidationId;

    @Column(name = "country_of_issue")
    @MasterData(type = MasterDataType.COUNTRIES)
    private String countryOfIssue;

    @Column(name = "type")
    @MasterData(type = MasterDataType.REFERENCE_NUMBER_TYPE)
    private String type;

    @Column(name = "reference_number")
    private String referenceNumber;

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Column(name = "booking_id")
    private Long bookingId;

    @Column(name = "is_portal_enable")
    private Boolean isPortalEnable;
}
