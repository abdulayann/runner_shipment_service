package com.dpw.runner.shipment.services.reportingservice.Reports;

import com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.reportingservice.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.BookingCarriageModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShippingRequestOutModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.fasterxml.jackson.core.type.TypeReference;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

@Component
public class ShippingRequestOutReport extends IReport {

    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IV1Service v1Service;

    @Override
    public Map<String, Object> getData(Long id) throws RunnerException {
        ShippingRequestOutModel shippingRequestOutModel = (ShippingRequestOutModel) getDocumentModel(id);
        return populateDictionary(shippingRequestOutModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) throws RunnerException {
        ShippingRequestOutModel model = new ShippingRequestOutModel();
        model.setTenant(getTenant());
        model.setConsolidation(getConsolidation(id));
        model.setUser(UserContext.getUser());

        var shipments = model.getConsolidation() != null ? model.getConsolidation().getShipmentsList() : null;
        if (model.getConsolidation() == null) {
            throw new RunnerException("Consolidation is null for the id " + id);
        }
        var allCommonContainers = new ArrayList<ContainerModel>();

        if (shipments != null && shipments.size() > 0) {
            for (var shipment : shipments) {
                var shipmentsContainersList = new ArrayList<ShipmentContainers>();
                var commonContainersList = shipment.getContainersList();
                if (commonContainersList != null && commonContainersList.size() > 0) {
                    for (var item : commonContainersList) {
                        var shipmentContainerRow = getShipmentContainer(item);
                        shipmentsContainersList.add(shipmentContainerRow);
                    }
                    allCommonContainers.addAll(commonContainersList);
                    shipment.setShipmentContainersList(shipmentsContainersList);
                }
            }
        } else {
            if (model.getConsolidation() != null)
                allCommonContainers.addAll(model.getConsolidation().getContainersList());
        }

        model.setShipmentList(shipments);

        if (model.getShipmentList() != null && model.getShipmentList().size() > 0) {
            model.setShipment(model.getShipmentList().get(0));
        }

        var consolContainers = new ArrayList<ShipmentContainers>();
        var tenantSettings = getShipmentSettings();

        if (tenantSettings.getIsShipmentLevelContainer() != null && tenantSettings.getIsShipmentLevelContainer().equals(true)) {
            if (model.getShipment() != null) {
                model.setCommonContainers(model.getShipment().getContainersList());
            } else {
                model.setCommonContainers(model.getConsolidation().getContainersList());
            }
            for (var container_row : model.getCommonContainers()) {
                consolContainers.add(getShipmentContainer(container_row));
            }
        } else {
            if (model.getShipmentList() != null && model.getShipmentList().size() > 0) {
                for (var shipment : model.getShipmentList()) {
                    if (shipment != null && shipment.getShipmentContainersList() != null) {
                        consolContainers.addAll(shipment.getShipmentContainersList());
                    }
                }
            }
            model.setCommonContainers(allCommonContainers);
        }

        model.setConsolContainers(consolContainers);

        if (model.getShipment() != null) {
            //setting serviceMode master data
            var masterData = getMasterListData(MasterDataType.SERVICE_MODE, model.getShipment().getServiceType());
            model.setServiceMode(masterData != null ? masterData.getItemDescription() : null);
            if(model.getShipment().getCarrierDetails() != null) {
                model.setLoadingPort(getUNLocRow(model.getShipment().getCarrierDetails().getOriginPort()));
                model.setDischargePort(getUNLocRow(model.getShipment().getCarrierDetails().getDestinationPort()));
                //setting carrier master data
                model.setCarrier(getCarrier(model.getShipment().getCarrierDetails().getShippingLine()));
            }
        }

        //setting shipment and container response
        model.setShipmentAndContainer(getShipmentAndContainerResponse(model.getShipmentList()));

        List<ShipmentContainers> commonContainers = new ArrayList<>();
        Map<String, ContainerModel> map = new HashMap<>();
        if (model.getConsolidation().getContainersList() != null) {
            for (ContainerModel containerModel : model.getConsolidation().getContainersList()) {
                map.put(containerModel.getContainerNumber(), containerModel);
            }
        }
        if (model.getShipment() != null && model.getShipment().getContainersList() != null) {
            for (ContainerModel container : model.getShipment().getContainersList()) {
                ShipmentContainers shipmentContainer = getShipmentContainer(container);
                shipmentContainer.BL_SealNumber = container.getCustomsSealNumber();
                if (map.containsKey(container.getContainerNumber()))
                    commonContainers.add(shipmentContainer);
            }
        }
        model.setCommonContainers(allCommonContainers);
        //setting vessel
        List<BookingCarriageModel> bookingCarriages = model.getShipment() != null ? model.getShipment().getBookingCarriagesList() : null;
        BookingCarriageModel bookingCarriage = null;
        if (bookingCarriages != null) {
            for (int i = 0; i < bookingCarriages.size(); i++) {
                if (Objects.equals(bookingCarriages.get(i).getCarriageType(), "PreCarriage")) {
                    bookingCarriage = bookingCarriages.get(i);
                    break;
                }
            }
        }
        if (bookingCarriage != null) {
            String vessel = bookingCarriage.getVessel();
            List<Object> vesselCriteria = Arrays.asList(
                    Arrays.asList(Constants.VESSEL_GUID_V1),
                    "=",
                    vessel
            );
            CommonV1ListRequest vesselRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(vesselCriteria).build();
            V1DataResponse vesselResponse = v1Service.fetchVesselData(vesselRequest);
            List<VesselsResponse> vesselsResponse = jsonHelper.convertValueToList(vesselResponse.entities, VesselsResponse.class);
            if (vesselsResponse != null && vesselsResponse.size() > 0)
                model.setVessel(vesselsResponse.get(0));
        }

        return model;
    }


    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        ShippingRequestOutModel model = (ShippingRequestOutModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();
        populateShipmentFields(model.getShipment(), dictionary);
        populateUserFields(model.getUser(), dictionary);
        populateConsolidationFields(model.getConsolidation(), dictionary);
        if (model.getLoadingPort() != null) {
            var loadingPort = model.getLoadingPort();
            dictionary.put(ReportConstants.LOADING_PORT_NAME, loadingPort.getName());
            dictionary.put(ReportConstants.LOADING_PORT_COUNTRY, loadingPort.getCountry());
            dictionary.put(ReportConstants.LOADING_PORT_CODE, loadingPort.getLocCode());
            if (loadingPort.getCountry() != null) {
                dictionary.put(ReportConstants.LOADING_PORT_COUNTRY_NAME, loadingPort.getCountry().toUpperCase());
            }
        }
        if (model.getDischargePort() != null) {
            var dischargePort = model.getDischargePort();
            dictionary.put(ReportConstants.DISCHARGE_PORT_NAME, dischargePort.getName());
            dictionary.put(ReportConstants.DISCHARGE_PORT_COUNTRY, dischargePort.getCountry());
            dictionary.put(ReportConstants.DISCHARGE_PORT_CODE, dischargePort.getLocCode());
            if (dischargePort.getCountry() != null) {
                dictionary.put(ReportConstants.DISCHARGE_PORT_COUNTRY_NAME, dischargePort.getCountry().toUpperCase());
            }
        }
        dictionary.put(ReportConstants.CONTAINER_COUNT_BY_CODE, getCountByCommonContainerTypeCode(model.getCommonContainers()));

        if (model.getConsolContainers() != null && !model.getConsolContainers().isEmpty()) {
            List<Map<String, Object>> containerSummary = model.getConsolContainers().stream()
                    .sorted(Comparator.comparing(ShipmentContainers::getContainerTypeCode))
                    .collect(Collectors.groupingBy(ShipmentContainers::getContainerTypeCode, Collectors.counting()))
                    .entrySet()
                    .stream()
                    .filter(Objects::nonNull)
                    .map(entry -> {
                        Map<String, Object> map = new HashMap<>();
                        map.put("key", entry.getKey());
                        map.put("ContCount", entry.getValue());
                        map.put("CountDesc", entry.getKey() + " * " + entry.getValue());
                        map.put("ContainerSummary", map.get("CountDesc"));
                        return map;
                    })
                    .toList();
            dictionary.put(ReportConstants.CONSOL_CONTAINERSUMMARY, containerSummary);
        }

        dictionary.put(ReportConstants.TOTAL_PACKS, ReportHelper.addCommaWithoutDecimal(BigDecimal.valueOf(getTotalPacks(model.getShipment()))));
        String unitCode = getTotalPackUnitCode(model.getShipmentList());
        dictionary.put(ReportConstants.TOTAL_PACK_UNIT_CODE, unitCode);
        dictionary.put(ReportConstants.TOTAL_PACK_UNIT_DESCRIPTION, masterListDescriptionPacksUnit(unitCode));

        dictionary.put(ReportConstants.CONSOL_CONTAINERS, model.getConsolContainers());
        if (model.getCarrier() != null) {
            dictionary.put(ReportConstants.CONSOL_CARRIER, model.getCarrier().getItemDescription());
            if (model.getCarrier().getCarrierContactPerson() != null)
                dictionary.put(ReportConstants.CARRIER_CONTACT_PERSON, model.getCarrier().getCarrierContactPerson().toUpperCase());
            if (model.getCarrier().getItemDescription() != null)
                dictionary.put(ReportConstants.CARRIER_NAME, model.getCarrier().getItemDescription().toUpperCase());
        }
        dictionary.put(ReportConstants.SHIPMENT_AND_CONTAINER, model.getShipmentAndContainer());
        if (model.getShipmentAndContainer() != null && model.getShipmentAndContainer().size() > 0) {
            dictionary.put(ReportConstants.CONSOL_DESCRIPTION_ENABLED, false);
            dictionary.put(ReportConstants.SHIPMENT_HASCONTAINERS, true);
        } else {
            dictionary.put(ReportConstants.CONSOL_DESCRIPTION_ENABLED, true);
            dictionary.put(ReportConstants.SHIPMENT_HASCONTAINERS, false);
        }

        List<Map<String, Object>> valuesContainer = jsonHelper.convertValue(dictionary.get(ReportConstants.SHIPMENT_AND_CONTAINER), new TypeReference<>() {});

        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        valuesContainer.forEach(v -> {
            if (v.containsKey(ReportConstants.WEIGHT))
                v.put(ReportConstants.WEIGHT, ConvertToWeightNumberFormat(v.get(ReportConstants.WEIGHT), v1TenantSettingsResponse));
            if (v.containsKey(ReportConstants.VOLUME))
                v.put(ReportConstants.VOLUME, ConvertToVolumeNumberFormat(v.get(ReportConstants.VOLUME), v1TenantSettingsResponse));
            if (v.containsKey(ReportConstants.PACKS))
                v.put(ReportConstants.PACKS, addCommas(v.get(ReportConstants.PACKS).toString()));
            if (model.getShipment() != null && model.getShipment().getPacksUnit() != null)
                v.put(ReportConstants.SHIPMENT_PACKS_UNIT_DESC, getMasterListData(MasterDataType.PACKS_UNIT, model.getShipment().getPacksUnit().toUpperCase()));
        });

        dictionary.put(ReportConstants.SHIPMENT_AND_CONTAINER, valuesContainer);
        if (model.getServiceMode() != null) {
            dictionary.put(ReportConstants.SERVICE_MODE_DESCRIPTION,
                    model.getServiceMode().toUpperCase());
        }
        if (model.getShipment() != null && model.getShipment().getServiceType() != null) {
            dictionary.put(ReportConstants.SERVICE_MODE, model.getShipment().getServiceType().toUpperCase());
        }
        List<String> shipmentIds = model.getShipmentList() != null ? model.getShipmentList().stream()
                .map(i -> i.getId().toString())
                .toList() : Collections.emptyList();
        dictionary.put(ReportConstants.SHIPMENT_IDS, String.join(",", shipmentIds));
        return dictionary;
    }

    private String masterListDescriptionPacksUnit(String packageType) {
        if (packageType == null || packageType.isEmpty())
            return packageType;

        MasterData masterData = getMasterListData(MasterDataType.PAYMENT_TYPE, packageType);
        return (masterData != null ? masterData.getItemDescription() : null);
    }

}
