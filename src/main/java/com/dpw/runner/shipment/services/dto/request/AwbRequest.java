package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.awb.*;
import com.dpw.runner.shipment.services.entity.enums.AwbStatus;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.List;
import java.util.UUID;

@Data
@ApiModel("Awb Request Model")
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AwbRequest implements IRunnerRequest {
    private Integer id;
    private UUID guid;
    private Long shipmentId;
    private Long consolidationId;
    private String awbNumber;
    private AwbShipmentInfo awbShipmentInfo;
    private List<AwbNotifyPartyInfo> awbNotifyPartyInfo;
    private List<AwbRoutingInfo> awbRoutingInfo;
    private AwbCargoInfo awbCargoInfo;
    private AwbPaymentInfo awbPaymentInfo;
    private List<AwbOtherChargesInfo> awbOtherChargesInfo;
    private AwbOtherInfo awbOtherInfo;
    private List<AwbOCIInfo> awbOciInfo;
    private List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfo;
    private List<AwbPackingInfo> awbPackingInfo;
    private List<AwbSpecialHandlingCodesMappingInfo> awbSpecialHandlingCodesMappings;
    private Boolean airMessageResubmitted;

    //Used to provide special handling codes for UI
    private List<String> shcIdList;
    private AwbStatus airMessageStatus;
    private AwbStatus linkedHawbAirMessageStatus;
}
