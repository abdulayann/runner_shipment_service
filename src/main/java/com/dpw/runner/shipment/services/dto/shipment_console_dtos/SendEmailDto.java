package com.dpw.runner.shipment.services.dto.shipment_console_dtos;

import com.dpw.runner.shipment.services.reportingservice.Models.TenantModel;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;
import java.util.Set;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SendEmailDto {
    private ShipmentDetails shipmentDetails;
    private ConsolidationDetails consolidationDetails;
    private ShipmentRequestedType type;
    private String rejectRemarks;
    private Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequestMap;
    private Set<ShipmentRequestedType> shipmentRequestedTypes;
    private Map<String, UnlocationsResponse> unLocMap;
    private Map<String, CarrierMasterData> carrierMasterDataMap;
    private Map<String, String> usernameEmailsMap;
    private Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap;
    private String requestedUser;
    private Map<Integer, TenantModel> tenantModelMap;
}
