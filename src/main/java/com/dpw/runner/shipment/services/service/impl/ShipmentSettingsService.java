package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ShipmentSettingRequest;
import com.dpw.runner.shipment.services.dto.response.ShipmentSettingsDetailsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentSettingsService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class ShipmentSettingsService implements IShipmentSettingsService {

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) throws Exception {
        ShipmentSettingRequest request = null;
        request = (ShipmentSettingRequest) commonRequestModel.getData();
        // TODO- implement validator
        ShipmentSettingsDetails shipmentSettingsDetails = convertRequestToEntity(request);
        shipmentSettingsDetails = shipmentSettingsDao.save(shipmentSettingsDetails);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(shipmentSettingsDetails));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception {
        ShipmentSettingRequest request = (ShipmentSettingRequest) commonRequestModel.getData();
        // TODO- implement Validation logic
        long id = request.getId();
        Optional<ShipmentSettingsDetails> oldEntity = shipmentSettingsDao.findById(id);
        if(!oldEntity.isPresent()) {
            log.debug("Shipment Setting is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ShipmentSettingsDetails shipmentSettingsDetails = convertRequestToEntity(request);
        shipmentSettingsDetails.setId(oldEntity.get().getId());
        shipmentSettingsDetails = shipmentSettingsDao.save(shipmentSettingsDetails);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(shipmentSettingsDetails));
    }

    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            long id = request.getId();
            Optional<ShipmentSettingsDetails> shipmentSettingsDetails = shipmentSettingsDao.findById(id);
            if(!shipmentSettingsDetails.isPresent()) {
                log.debug("Shipment Setting is null for Id {}", id);
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            ShipmentSettingsDetailsResponse response = convertEntityToDto(shipmentSettingsDetails.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            // TODO- implement actual logic with filters
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            Pair<Specification<ShipmentSettingsDetails>, Pageable> tuple = fetchData(request, ShipmentSettingsDetails.class.getSimpleName(), tableNames);
            Page<ShipmentSettingsDetails> shipmentSettingsPage  = shipmentSettingsDao.findAll(tuple.getLeft(), tuple.getRight());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(shipmentSettingsPage.getContent()),
                    shipmentSettingsPage.getTotalPages(),
                    shipmentSettingsPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }
    public static ShipmentSettingsDetails convertRequestToEntity(ShipmentSettingRequest request) {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setHouseBillNumberLock(request.getHouseBillNumberLock());
        shipmentSettingsDetails.setGuid(request.getGuid());
        shipmentSettingsDetails.setRestrictHblGen(request.getRestrictHblGen());
        shipmentSettingsDetails.setPrintPhoneNumber(request.getPrintPhoneNumber());
        shipmentSettingsDetails.setHousebillPrefix(request.getHousebillPrefix());
        shipmentSettingsDetails.setHousebillNumberGeneration(request.getHousebillNumberGeneration());
        shipmentSettingsDetails.setFooterColumns(request.getFooterColumns());
        shipmentSettingsDetails.setIsAutoPopulateShipType(request.getIsAutoPopulateShipType());
        shipmentSettingsDetails.setPartialCloningEnabled(request.getPartialCloningEnabled());
        shipmentSettingsDetails.setShipConsolidationContainerEnabled(request.getShipConsolidationContainerEnabled());
        shipmentSettingsDetails.setMultipleShipmentEnabled(request.getMultipleShipmentEnabled());
        shipmentSettingsDetails.setEnableRouteMaster(request.getEnableRouteMaster());
        shipmentSettingsDetails.setArApFlag(request.getArApFlag());
        shipmentSettingsDetails.setShipmentTiCargesLinkage(request.getShipmentTiCargesLinkage());
        shipmentSettingsDetails.setCooldownTime(request.getCooldownTime());
        shipmentSettingsDetails.setAdvancePeriod(request.getAdvancePeriod());
        shipmentSettingsDetails.setAutoEventCreate(request.getAutoEventCreate());
        shipmentSettingsDetails.setShipmentLite(request.getShipmentLite());
        shipmentSettingsDetails.setBillingLite(request.getBillingLite());
        shipmentSettingsDetails.setRestrictedLocationsEnabled(request.getRestricted_Locations_Enabled());
        shipmentSettingsDetails.setIsAtdAtaAutoPopulateEnabled(request.getIsAtdAtaAutoPopulateEnabled());
        shipmentSettingsDetails.setRestrictedLocations(request.getRestricted_Locations());
        shipmentSettingsDetails.setShipmentImportApproverRole(request.getShipmentImportApproverRole());
        return shipmentSettingsDetails;
    }

    private static ShipmentSettingsDetailsResponse convertEntityToDto(ShipmentSettingsDetails shipmentSettingsDetails) {
        ShipmentSettingsDetailsResponse response = new ShipmentSettingsDetailsResponse();
        response.setId(shipmentSettingsDetails.getId());
        response.setGuid(shipmentSettingsDetails.getGuid());
        response.setHouseBillNumberLock(shipmentSettingsDetails.getHouseBillNumberLock());
        response.setRestrictHblGen(shipmentSettingsDetails.getRestrictHblGen());
        response.setPrintPhoneNumber(shipmentSettingsDetails.getPrintPhoneNumber());
        response.setHousebillPrefix(shipmentSettingsDetails.getHousebillPrefix());
        response.setHousebillNumberGeneration(shipmentSettingsDetails.getHousebillNumberGeneration());
        response.setFooterColumns(shipmentSettingsDetails.getFooterColumns());
        response.setIsAutoPopulateShipType(shipmentSettingsDetails.getIsAutoPopulateShipType());
        response.setPartialCloningEnabled(shipmentSettingsDetails.getPartialCloningEnabled());
        response.setShipConsolidationContainerEnabled(shipmentSettingsDetails.getShipConsolidationContainerEnabled());
        response.setMultipleShipmentEnabled(shipmentSettingsDetails.getMultipleShipmentEnabled());
        response.setEnableRouteMaster(shipmentSettingsDetails.getEnableRouteMaster());
        response.setArApFlag(shipmentSettingsDetails.getArApFlag());
        response.setShipmentTiCargesLinkage(shipmentSettingsDetails.getShipmentTiCargesLinkage());
        response.setCooldownTime(shipmentSettingsDetails.getCooldownTime());
        response.setAdvancePeriod(shipmentSettingsDetails.getAdvancePeriod());
        response.setAutoEventCreate(shipmentSettingsDetails.getAutoEventCreate());
        response.setShipmentLite(shipmentSettingsDetails.getShipmentLite());
        response.setBillingLite(shipmentSettingsDetails.getBillingLite());
        response.setRestricted_Locations_Enabled(shipmentSettingsDetails.getRestrictedLocationsEnabled());
        response.setIsAtdAtaAutoPopulateEnabled(shipmentSettingsDetails.getIsAtdAtaAutoPopulateEnabled());
        response.setRestricted_Locations(shipmentSettingsDetails.getRestrictedLocations());
        response.setShipmentImportApproverRole(shipmentSettingsDetails.getShipmentImportApproverRole());
        return response;
    }

    private static List<IRunnerResponse> convertEntityListToDtoList(List<ShipmentSettingsDetails> list) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        list.forEach(shipmentSettingsDetail -> {
            responseList.add(convertEntityToDto(shipmentSettingsDetail));
        });
        return responseList;
    }

    private static Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("houseBillNumberLock", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(Boolean.class).build()),
            Map.entry("restrictHblGen", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(Boolean.class).build()),
            Map.entry("printPhoneNumber", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(Boolean.class).build()),
            Map.entry("housebillPrefix", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(String.class).build()),
            Map.entry("housebillNumberGeneration", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(String.class).build()),
            Map.entry("footerColumns", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(Integer.class).build()),
            Map.entry("isAutoPopulateShipType", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(Boolean.class).build()),
            Map.entry("partialCloningEnabled", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(Boolean.class).build()),
            Map.entry("shipConsolidationContainerEnabled", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(Boolean.class).build()),
            Map.entry("multipleShipmentEnabled", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(Boolean.class).build()),
            Map.entry("enableRouteMaster", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(Boolean.class).build()),
            Map.entry("arApFlag", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(Boolean.class).build()),
            Map.entry("shipmentTiCargesLinkage", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(Boolean.class).build()),
            Map.entry("cooldownTime", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(Integer.class).build()),
            Map.entry("advancePeriod", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(Integer.class).build()),
            Map.entry("autoEventCreate", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(Boolean.class).build()),
            Map.entry("shipmentLite", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(Boolean.class).build()),
            Map.entry("billingLite", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(Boolean.class).build()),
            Map.entry("restrictedLocationsEnabled", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(Boolean.class).build()),
            Map.entry("isAtdAtaAutoPopulateEnabled", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(Boolean.class).build()),
            Map.entry("restrictedLocations", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(List.class).build()),
            Map.entry("shipmentImportApproverRole", RunnerEntityMapping.builder().tableName("ShipmentSettingsDetails").dataType(String.class).build())
    );
}
