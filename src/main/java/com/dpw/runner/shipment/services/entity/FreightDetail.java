package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.PayerType;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "freight_details")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class FreightDetail extends MultiTenancy {

    @Column(name = "charge_type")
    @MasterData(type = MasterDataType.BOOKING_CHARGES)
    private String chargeType;

    @Column(name = "payment_terms")
    @MasterData(type = MasterDataType.PAYMENT)
    private String paymentTerms;

    @Column(name = "payer_type")
    private PayerType payerType;

    @Column(name = "payer_location")
    @UnlocationData
    private String payerLocation;

    @Column(name = "shipping_instruction_id")
    private Long shippingInstructionId;
}
