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
    public List<AwbNotifyPartyInfo> awbNotifyPartyInfo;
    public List<AwbRoutingInfoResponse> awbRoutingInfo;
    public AwbCargoInfo awbCargoInfo;
    public AwbPaymentInfo awbPaymentInfo;
    public List<AwbOtherChargesInfo> awbOtherChargesInfo;
    public AwbOtherInfo awbOtherInfo;
    public List<AwbOCIInfo> awbOciInfo;
    public List<AwbGoodsDescriptionInfoResponse> awbGoodsDescriptionInfo;
    public List<AwbPackingInfoResponse> awbPackingInfo;
    public List<AwbSpecialHandlingCodesMappingInfo> awbSpecialHandlingCodesMappings;
    public AwbShipConsoleDto awbKafkaEntity;
    public Map<String, String> masterData;
    public Map<String, String> unlocationData;
    public MasterData chargeDetails;
    // Default Awb objects for UI
    private AwbShipmentInfoResponse defaultAwbShipmentInfo;

}
