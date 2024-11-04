package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.enums.OceanChargeType;
import com.dpw.runner.shipment.services.entity.enums.PayerParties;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.*;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;

@Entity
@Data
@Table(name = "booking_payment")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SQLDelete(sql = "UPDATE booking_payment SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class BookingPayment extends MultiTenancy {

    @Enumerated(EnumType.STRING)
    @Column(name = "ocean_charge_type")
    private OceanChargeType oceanChargeType;

    @Column(name = "charge_type")
    @DedicatedMasterData(type = Constants.CHARGE_TYPE_MASTER_DATA)
    private String chargeType; //charge type master data

    @Column(name = "payment_terms")
    @MasterData(type = MasterDataType.PAYMENT)
    private String paymentTerms;

    @Enumerated(EnumType.STRING)
    @Column(name = "payer")
    private PayerParties payer;

    @Column(name = "payment_location")
    @UnlocationData
    private String paymentLocation;

    @Column(name = "carrier_booking_id")
    private Long carrierBookingId;

}
