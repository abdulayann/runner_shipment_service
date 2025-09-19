package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.dao.interfaces.ITransactionHistoryDao;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.bridgeService.BridgeServiceResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.TransactionHistory;
import com.dpw.runner.shipment.services.entity.enums.EntityTypeTransactionHistory;
import com.dpw.runner.shipment.services.entity.enums.FlowType;
import com.dpw.runner.shipment.services.entity.enums.SourceSystem;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.Objects;

@Slf4j
@Component
public class CarrierBookingInttraUtil {

    @Autowired
    private ITransactionHistoryDao transactionHistoryDao;

    public boolean isBridgeServiceResponseNotValid(BridgeServiceResponse bridgeServiceResponse) {
        return bridgeServiceResponse.getExtraResponseParams().containsKey(AwbConstants.SERVICE_HTTP_STATUS_CODE) && !Objects.equals(bridgeServiceResponse.getExtraResponseParams().get(AwbConstants.SERVICE_HTTP_STATUS_CODE).toString(), "200") &&
                !Objects.equals(bridgeServiceResponse.getExtraResponseParams().get(AwbConstants.SERVICE_HTTP_STATUS_CODE).toString(), "400");
    }

    public void createTransactionHistory(String actionStatus, FlowType flowType, String description, SourceSystem sourceSystem, Long id, EntityTypeTransactionHistory entityType) {
        TransactionHistory transactionHistory = TransactionHistory.builder()
                .actionStatusDescription(actionStatus)
                .flowType(flowType)
                .description(description)
                .sourceSystem(sourceSystem)
                .actualDateTime(LocalDateTime.now())
                .entityType(entityType)
                .entityId(id)
                .build();
        transactionHistoryDao.save(transactionHistory);
    }

    public PartiesResponse fetchRequiredParty(Parties party) {
        if (Objects.isNull(party)) {
            return null;
        }

        return PartiesResponse.builder()
                .id(party.getId())
                .entityId(party.getEntityId())
                .entityType(party.getEntityType())
                .type(party.getType())
                .orgCode(party.getOrgCode())
                .tenantId(party.getTenantId())
                .addressCode(party.getAddressCode())
                .orgId(party.getOrgId())
                .addressId(party.getAddressId())
                .orgData(party.getOrgData())
                .addressData(party.getAddressData())
                .isAddressFreeText(party.getIsAddressFreeText())
                .countryCode(party.getCountryCode())
                .build();
    }
}
