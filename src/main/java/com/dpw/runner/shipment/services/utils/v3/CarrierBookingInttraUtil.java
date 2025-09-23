package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.adapters.impl.BridgeServiceAdapter;
import com.dpw.runner.shipment.services.dao.interfaces.ITransactionHistoryDao;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.bridgeService.BridgeServiceResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.TransactionHistory;
import com.dpw.runner.shipment.services.entity.enums.EntityTypeTransactionHistory;
import com.dpw.runner.shipment.services.entity.enums.FlowType;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.SourceSystem;
import com.dpw.runner.shipment.services.entity.enums.Status;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.migration.utils.MigrationUtil;
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

    @Autowired
    BridgeServiceAdapter bridgeServiceAdapter;

    @Autowired
    MigrationUtil migrationUtil;

    @Autowired
    JsonHelper jsonHelper;

    public <T> void sendPayloadToBridge(T inttraResponse, Long id,
                                         String integrationCode, String transactionId, String referenceId,
                                         IntegrationType integrationType, String entityType) throws RunnerException {
        String bridgePayload = jsonHelper.convertToJson(inttraResponse);
        log.info("Bridge payload {}", bridgePayload);
        BridgeServiceResponse bridgeServiceResponse;
        try {
            bridgeServiceResponse = (BridgeServiceResponse) bridgeServiceAdapter.bridgeApiIntegration(inttraResponse, integrationCode, transactionId, referenceId);
        } catch (Exception exception) {
            log.error("Getting error from Bridge while uploading template to: " + exception);
            migrationUtil.saveErrorResponse(id, entityType, integrationType, Status.FAILED, exception.getMessage());
            throw new RunnerException("Getting error from Bridge");
        }
        migrationUtil.saveErrorResponse(id, entityType, integrationType, Status.SUCCESS, bridgeServiceResponse.toString());
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
