package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;

import static com.dpw.runner.shipment.services.entity.enums.GenerationType.Random;

@Component
@Slf4j
public class ConsolidationCommonUtils {

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private ProductIdentifierUtility productEngine;

    @Autowired
    private GetNextNumberHelper getNextNumberHelper;

    @Autowired
    private IEventDao eventDao;


    public String generateCustomBolNumber() {
        String res = null;
        ShipmentSettingsDetails tenantSetting = commonUtils.getShipmentSettingFromContext();

        if(tenantSetting.getConsolidationLite() != null && tenantSetting.getConsolidationLite() && tenantSetting.getBolNumberGeneration() == null) {
            return  res;
        }

        if(tenantSetting.getBolNumberGeneration() != null) {
            res = tenantSetting.getBolNumberPrefix() != null ? tenantSetting.getBolNumberPrefix() : "";
            if (tenantSetting.getBolNumberGeneration() == Random) {
                res += StringUtility.getRandomString(10);
            } else {
                String serialNumber = v1Service.getMaxConsolidationId();
                res += serialNumber;
            }

        }

        return res;
    }

    public String getCustomizedConsolidationProcessNumber(ConsolidationDetails consolidationDetails, ProductProcessTypes productProcessTypes) throws RunnerException {
        List<TenantProducts> enabledTenantProducts = productEngine.populateEnabledTenantProducts();
        if (productProcessTypes == ProductProcessTypes.ReferenceNumber) {
            // to check the commmon sequence
            var sequenceNumber = productEngine.getCommonSequenceNumber(consolidationDetails.getTransportMode(), ProductProcessTypes.Consol_Shipment_TI);
            if (sequenceNumber != null && !sequenceNumber.isEmpty()) {
                return sequenceNumber;
            }
        }
        var identifiedProduct = productEngine.identifyProduct(consolidationDetails, enabledTenantProducts);
        if (identifiedProduct == null){
            return "";
        }
        var sequenceSettings = getNextNumberHelper.getProductSequence(identifiedProduct.getId(), productProcessTypes);
        if(sequenceSettings == null){
            return "";
        }
        String prefix = sequenceSettings.getPrefix() == null ? "" : sequenceSettings.getPrefix();
        var user = UserContext.getUser();
        return getNextNumberHelper.generateCustomSequence(sequenceSettings, prefix, user.getTenantId(), true, null, false);
    }

    public void setReceivingAndTriangulationBranch(ConsolidationDetails consolidationDetails) {
        if(consolidationDetails.getReceivingBranch() != null && consolidationDetails.getReceivingBranch() == 0)
            consolidationDetails.setReceivingBranch(null);
        if(ObjectUtils.isNotEmpty(consolidationDetails.getTriangulationPartnerList())
                && consolidationDetails.getTriangulationPartnerList().size() == 1) {
            TriangulationPartner triangulationPartner = consolidationDetails.getTriangulationPartnerList().get(0);
            if (triangulationPartner != null && Long.valueOf(0).equals(triangulationPartner.getTriangulationPartner()))
                consolidationDetails.setTriangulationPartnerList(null);
        } else if (consolidationDetails.getTriangulationPartnerList() == null
                && consolidationDetails.getTriangulationPartner() != null
                && consolidationDetails.getTriangulationPartner() == 0) {
            consolidationDetails.setTriangulationPartner(null);
        }
    }

    public Events createEvent(ConsolidationDetails consolidationDetails, String eventCode) {
        Events events = new Events();
        // Set event fields from consolidation
        events.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
        events.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);
        events.setIsPublicTrackingEvent(true);
        events.setEntityType(Constants.CONSOLIDATION);
        events.setEntityId(consolidationDetails.getId());
        events.setTenantId(TenantContext.getCurrentTenant());
        events.setEventCode(eventCode);
        events.setConsolidationId(consolidationDetails.getId());
        events.setDirection(consolidationDetails.getShipmentType());
        // Persist the event
        eventDao.save(events);
        return events;
    }

    public void generateConsolidationNumber(ConsolidationDetails consolidationDetails) throws RunnerException {
        Boolean customisedSequence = shipmentSettingsDao.getCustomisedSequence();

        if(consolidationDetails.getConsolidationNumber() == null) {
            if(Boolean.TRUE.equals(customisedSequence)) {
                String consoleNumber = getCustomizedConsolidationProcessNumber(consolidationDetails, ProductProcessTypes.ReferenceNumber);
                if(consoleNumber != null && !consoleNumber.isEmpty())
                    consolidationDetails.setConsolidationNumber(consoleNumber);
                setConsolidationNumber(consolidationDetails);
                setReferenceNumber(consolidationDetails);
            }
            else {
                consolidationDetails.setConsolidationNumber("CONS000" + getConsolidationSerialNumber());
                setReferenceNumber(consolidationDetails);
            }
        }

        setBolConsolidation(consolidationDetails);
    }

    private void setConsolidationNumber(ConsolidationDetails consolidationDetails) {
        if (consolidationDetails.getConsolidationNumber() == null || consolidationDetails.getConsolidationNumber().isEmpty())
            consolidationDetails.setConsolidationNumber("CONS000" + getConsolidationSerialNumber());
    }

    private String getConsolidationSerialNumber() {
        return v1Service.getMaxConsolidationId();
    }

    private void setReferenceNumber(ConsolidationDetails consolidationDetails) {
        if (consolidationDetails.getReferenceNumber() == null || consolidationDetails.getReferenceNumber().isEmpty())
            consolidationDetails.setReferenceNumber(consolidationDetails.getConsolidationNumber());
    }

    private void setBolConsolidation(ConsolidationDetails consolidationDetails) throws RunnerException {
        if (StringUtility.isEmpty(consolidationDetails.getBol()) && Objects.equals(commonUtils.getShipmentSettingFromContext().getConsolidationLite(), false)) {
            String bol = getCustomizedConsolidationProcessNumber(consolidationDetails, ProductProcessTypes.BOLNumber);
            if (StringUtility.isEmpty(bol)) {
                bol = generateCustomBolNumber();
            }
            if (StringUtility.isNotEmpty(bol)) {
                consolidationDetails.setBol(bol);
            }
        }
    }

}
