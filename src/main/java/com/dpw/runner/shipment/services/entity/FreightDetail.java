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
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;

@Entity
@Table(name = "freight_details")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE freight_details SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class FreightDetail extends MultiTenancy {

    @Column(name = "charge_type")
    @MasterData(type = MasterDataType.BOOKING_CHARGES)
    private String chargeType;

    @Column(name = "payment_terms")
    @MasterData(type = MasterDataType.PAYMENT)
    private String paymentTerms;

    @Column(name = "payer_type")
    @Enumerated(EnumType.STRING)
    private PayerType payerType;

    @Column(name = "payer_location")
    @UnlocationData
    private String payerLocation;

    @Column(name = "shipping_instruction_id")
    private Long shippingInstructionId;
}
