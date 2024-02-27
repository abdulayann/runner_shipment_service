package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.Kafka.Dto.AwbShipConsoleDto;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.awb.*;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import lombok.Data;

import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
public class AwbResponse implements IRunnerResponse {
    private Integer id;
    private UUID guid;
    private String awbNumber;
    private Long shipmentId;
    private Long consolidationId;
    private AwbShipmentInfoResponse awbShipmentInfo;
    private List<AwbNotifyPartyInfo> awbNotifyPartyInfo;
    private List<AwbRoutingInfoResponse> awbRoutingInfo;
    private AwbCargoInfo awbCargoInfo;
    private AwbPaymentInfo awbPaymentInfo;
    private List<AwbOtherChargesInfo> awbOtherChargesInfo;
    private AwbOtherInfoResponse awbOtherInfo;
    private List<AwbOCIInfo> awbOciInfo;
    private List<AwbGoodsDescriptionInfoResponse> awbGoodsDescriptionInfo;
    private List<AwbPackingInfoResponse> awbPackingInfo;
    private List<AwbSpecialHandlingCodesMappingInfo> awbSpecialHandlingCodesMappings;
    private AwbShipConsoleDto awbKafkaEntity;
    private Map<String, String> masterData;
    private Map<String, String> unlocationData;
    private MasterData chargeDetails;
    // Default Awb objects for UI
    private AwbShipmentInfoResponse defaultAwbShipmentInfo;
    private List<AwbNotifyPartyInfo> defaultAwbNotifyPartyInfo;
    private List<AwbRoutingInfoResponse> defaultAwbRoutingInfo;
    private String errors;
    //Used to provide special handling codes for UI
    private List<String> shcIdList;

}
