package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.enums.OceanChargeType;
import com.dpw.runner.shipment.services.entity.enums.PayerParties;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;

@Entity
@Getter
@Setter
@Table(name = "booking_payment")
@AllArgsConstructor
@NoArgsConstructor
public class BookingPayment extends MultiTenancy {

//    @OneToOne(targetEntity = CarrierBooking.class, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
//    @JoinColumn(name = "booking_id", referencedColumnName = "id")
    @OneToOne(cascade = CascadeType.ALL, fetch = FetchType.LAZY, mappedBy = "booking_id")
    private CarrierBooking carrierBooking; // todo: doubt here, one to one map question mapped

    @Enumerated(EnumType.STRING)
    @Column(name = "ocean_charge_type")
    private OceanChargeType oceanChargeType;

    @Column(name = "charges_type")
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

}
