package com.dpw.runner.booking.services.dto.response.OrderManagement;

import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;
import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class OrderManagementDTO implements IRunnerResponse {
    private String orderId;
    private UUID guid;
    private String orderNumber;
    private String splitNumber;
    private String confirmationNumber;
    private String invoiceNumber;
    private String orderType;

    private String orderDate;
    private String pickupDate;
    private String expectedDeliveryDate;
    private String confirmationDate;
    private String invoiceDate;

    private String goodsDescription;
    private String buyerCode;
    private String supplierCode;
    private String incoTerm;

    private String serviceMode;
    private String countryOfOriginCode;
    private String countryOfOriginName;
    private String containerMode;
    private String bookingId;
    private String shipmentId;
    private String orderState;
    private String transportMode;
    private Long tenantId;

    private String consolidationId;
    private String origin;
    private String destination;
    private String originPort;
    private String destinationPort;
    private String houseBillId;
    private String masterBillId;
    private String buyerAddressCode;
    private Map<String, Object> buyerAddress;
    private String supplierAddressCode;
    private Map<String, Object> supplierAddress;
    private String notifyPartyCode;
    private String sendingAgentCode;
    private String receivingAgentCode;
    private String notifyPartyAddressCode;
    private Map<String, Object> notifyPartyAddress;
    private String sendingAgentAddressCode;
    private Map<String, Object> sendingAgentAddress;
    private String receivingAgentAddressCode;
    private Map<String, Object> receivingAgentAddress;
    private String voyageId;
    private String carrierId;
    private QuantityPair currencyAmount;
    private QuantityPair packsAmount;
    private QuantityPair volumeAmount;
    private QuantityPair weightAmount;
    public String shipmentETA;
    public String shipmentETD;
    public String shipmentATA;
    public String shipmentATD;
    private String terminalCutOffDate;
    private List<OrderLineResponse> orderLines;
    private List<OrderContainerResponse> containers;
    private List<OrderEventsResponse> events;
    private List<OrderDocumentResponse> documents;
    private List<OrderManagementDTO> splitOrders;
    private List<ReferencesResponse> references;
    public Boolean isLocked;
    public String createdBy;

    private String originName;
    private String destinationName;
    private String originPortName;
    private String destinationPortName;
    private String buyerName;
    private String supplierName;
    
}
