package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import com.dpw.runner.shipment.services.entity.enums.PayerType;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import com.dpw.runner.shipment.services.utils.MasterData;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class FreightDetailRequest {

    @MasterData(type = MasterDataType.BOOKING_CHARGES)
    private String chargeType;

    @MasterData(type = MasterDataType.PAYMENT)
    private String paymentTerms;

    private PayerType payerType;

    @UnlocationData
    private String payerLocation;

    private Long shippingInstructionId;
}
