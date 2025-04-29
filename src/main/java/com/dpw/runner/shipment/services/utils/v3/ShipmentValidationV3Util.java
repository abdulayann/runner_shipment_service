package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.aspects.LicenseContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import static com.dpw.runner.shipment.services.commons.constants.Constants.CAN_NOT_ATTACH_MORE_SHIPMENTS_IN_DG_CONSOL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CAN_NOT_UPDATE_DG_SHIPMENTS_CONSOLE_CONSISTS_MULTIPLE_SHIPMENTS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_TYPE_LCL;
import static com.dpw.runner.shipment.services.utils.CommonUtils.listIsNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.setIsNullOrEmpty;

@Slf4j
@Component
public class ShipmentValidationV3Util {

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;


    public void validateStaleShipmentUpdateError(ShipmentDetails shipmentDetails, boolean isCreate) {
        if(!isCreate) {
            // Check the shipment for attached consolidation, if the user is updating stale shipment and causing shipment to detach
            List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByShipmentId(shipmentDetails.getId());
            if (!CollectionUtils.isEmpty(consoleShipmentMappings)) {
                consoleShipmentMappings = consoleShipmentMappings.stream().filter(i -> Boolean.TRUE.equals(i.getIsAttachmentDone())).toList();
                if (CollectionUtils.isEmpty(shipmentDetails.getConsolidationList()) && !consoleShipmentMappings.isEmpty()
                        && !Objects.isNull(consoleShipmentMappings.get(0).getRequestedType())) {
                    throw new ValidationException(ShipmentConstants.STALE_SHIPMENT_UPDATE_ERROR);
                }
            }
        }
    }

    public void validTransportModeForTrasnportModeConfig(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, boolean isImportFile, V1TenantSettingsResponse tenantSettings) {
        if (Boolean.TRUE.equals(tenantSettings.getTransportModeConfig()) && Boolean.FALSE.equals(isImportFile) && (isCreate || !Objects.equals(oldEntity.getTransportMode(), shipmentDetails.getTransportMode()))
                && Boolean.FALSE.equals(commonUtils.isTransportModeValid(shipmentDetails.getTransportMode(), Constants.SHIPMENT_DETAILS, tenantSettings))) {
            throw new ValidationException(String.format(ErrorConstants.INVALID_TRANSPORT_MODE, shipmentDetails.getTransportMode()));
        }
    }

    public void processDGValidations(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, Set<ConsolidationDetails> consolidationDetails) throws RunnerException {
        if (Constants.TRANSPORT_MODE_AIR.equals(shipmentDetails.getTransportMode()))
            airDGValidations(shipmentDetails, consolidationDetails);
        if (Boolean.TRUE.equals(shipmentDetails.getContainsHazardous()) && (Objects.isNull(oldEntity) || !Boolean.TRUE.equals(oldEntity.getContainsHazardous())) && !setIsNullOrEmpty(consolidationDetails)) {
            ConsolidationDetails consolidationDetails1 = consolidationDetails.iterator().next();
            dgValidations(shipmentDetails, consolidationDetails1, 0);
        }
    }

    private void airDGValidations(ShipmentDetails shipmentDetails, Set<ConsolidationDetails> consolidationDetails) throws RunnerException {
        if(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag()) && !isAirDgUser()
                && !Boolean.TRUE.equals(shipmentDetails.getContainsHazardous()) && (!setIsNullOrEmpty(consolidationDetails) && Boolean.TRUE.equals(consolidationDetails.iterator().next().getHazardous()))) {
            throw new RunnerException("You do not have Air DG permissions to edit this as it is a part of DG Consol");
        }
    }

    private void dgValidations(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails1, int isNewConsoleAttached) throws RunnerException {
        if( ((Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails1.getTransportMode()) && SHIPMENT_TYPE_LCL.equals(consolidationDetails1.getContainerCategory()))
                || checkForAirDGFlag(consolidationDetails1))
                && (Boolean.TRUE.equals(consolidationDetails1.getHazardous()) || Boolean.TRUE.equals(shipmentDetails.getContainsHazardous()))) {
            List<ConsoleShipmentMapping> consoleShipmentMapping = consoleShipmentMappingDao.findByConsolidationId(consolidationDetails1.getId());
            if(!listIsNullOrEmpty(consoleShipmentMapping) && consoleShipmentMapping.size() + isNewConsoleAttached > 1) {
                throwErrorMaxOneShipmentAllowedInDgConsolidation(consolidationDetails1, isNewConsoleAttached);
            }
        }
    }

    private void throwErrorMaxOneShipmentAllowedInDgConsolidation(ConsolidationDetails consolidationDetails1, int isNewConsoleAttached) throws RunnerException {
        if(Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails1.getTransportMode()))
            throw new RunnerException("For Ocean DG Consolidation LCL Cargo Type, and can have only 1 shipment");
        else {
            if(isNewConsoleAttached == 1)
                throw new RunnerException(String.format(CAN_NOT_ATTACH_MORE_SHIPMENTS_IN_DG_CONSOL, consolidationDetails1.getConsolidationNumber()));
            else
                throw new RunnerException(CAN_NOT_UPDATE_DG_SHIPMENTS_CONSOLE_CONSISTS_MULTIPLE_SHIPMENTS);
        }
    }

    private boolean isAirDgUser() {
        return  LicenseContext.isDgAirLicense();
    }

    private boolean checkForAirDGFlag(ConsolidationDetails consolidationDetails) {
        if(!Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag()))
            return false;
        return Constants.TRANSPORT_MODE_AIR.equals(consolidationDetails.getTransportMode());
    }
}
