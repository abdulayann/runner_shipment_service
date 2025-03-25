package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.kafka.dto.AwbShipConsoleDto;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.awb.*;
import com.dpw.runner.shipment.services.entity.enums.AwbStatus;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import lombok.Data;

import java.time.LocalDateTime;
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
    private Boolean acasEnabled;
    private AwbOCIInfo awbOCIInfo;
    private List<AwbGoodsDescriptionInfoResponse> awbGoodsDescriptionInfo;
    private List<AwbPackingInfoResponse> awbPackingInfo;
    private List<AwbSpecialHandlingCodesMappingInfo> awbSpecialHandlingCodesMappings;
    private AwbShipConsoleDto awbKafkaEntity;
    private Map<String, String> masterData;
    private Map<String, String> unlocationData;
    private MasterData chargeDetails;
    private Boolean airMessageResubmitted;
    // Default Awb objects for UI
    private AwbShipmentInfoResponse defaultAwbShipmentInfo;
    private List<AwbNotifyPartyInfo> defaultAwbNotifyPartyInfo;
    private List<AwbRoutingInfoResponse> defaultAwbRoutingInfo;
    private String errors;
    //Used to provide special handling codes for UI
    private List<String> shcIdList;
    private AwbStatus airMessageStatus;
    private AwbStatus linkedHawbAirMessageStatus;
    private LocalDateTime originalPrintedAt;
    private AirMessagingAdditionalFields airMessagingAdditionalFields;

    private String userDisplayName;
}
