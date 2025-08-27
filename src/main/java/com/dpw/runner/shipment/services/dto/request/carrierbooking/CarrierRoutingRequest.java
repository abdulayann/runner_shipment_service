package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import com.dpw.runner.shipment.services.utils.MasterData;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Size;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CarrierRoutingRequest {

    private Integer sequence;

    @MasterData(type = MasterDataType.TRANSPORT_MODE)
    private String transportMode;

    private RoutingCarriage carriageType;

    @UnlocationData
    private String pol;   // Port of Loading

    @UnlocationData
    private String pod;   // Port of Discharge

    private LocalDateTime eta;
    private LocalDateTime etd;

    @Size(max=2048, message = "max size is 2048 for vessel_name")
    @DedicatedMasterData(type = Constants.VESSEL_MASTER_DATA)
    private String vesselName;

    private String voyageNo;
    private Long carrierBookingId;
}
