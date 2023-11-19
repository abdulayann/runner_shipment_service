package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.BookingCarriageModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShippingRequestOutModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

@Component
public class ShippingRequestOutReport extends IReport {

    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IV1Service v1Service;

    @Override
    public Map<String, Object> getData(Long id) {
        ShippingRequestOutModel shippingRequestOutModel = (ShippingRequestOutModel) getDocumentModel(id);
        return populateDictionary(shippingRequestOutModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) {
        ShippingRequestOutModel model = new ShippingRequestOutModel();
        model.setTenant(getTenant());
        model.setShipment(getShipment(id));
        model.setConsolidation(getFirstConsolidationFromShipmentId(id));
        model.setShipmentList(model.getConsolidation().getShipmentsList());
        model.setLoadingPort(getUNLocRow(model.getShipment().getCarrierDetails().getOriginPort()));
        model.setDischargePort(getUNLocRow(model.getShipment().getCarrierDetails().getDestinationPort()));
        model.setUser(UserContext.getUser());
        model.setConsolContainers(model.getConsolidation().getContainersList());
        model.setShipmentPacking(model.getShipment().getPackingList());

        //setting serviceMode master data
        var masterData = getMasterListData(MasterDataType.SERVICE_MODE, model.getShipment().getServiceType());
        model.setServiceMode(masterData != null ? masterData.getItemDescription() : null);

        //setting carrier master data
        model.setCarrier(getCarrier(model.getShipment().getCarrierDetails().getShippingLine()));

        //setting shipment and container response
        model.setShipmentAndContainer(getShipmentAndContainerResponse(model.getShipmentList()));

        List<ShipmentContainers> commonContainers = new ArrayList<>();
        Map<String, ContainerModel> map = new HashMap<>();
        if (model.getConsolidation().getContainersList() != null) {
            for (ContainerModel containerModel : model.getConsolidation().getContainersList()) {
                map.put(containerModel.getContainerNumber(), containerModel);
            }
        }
        if (model.getShipment().getContainersList() != null) {
            for (ContainerModel container : model.getShipment().getContainersList()) {
                ShipmentContainers shipmentContainer = getShipmentContainer(container);
                shipmentContainer.BL_SealNumber = container.getCustomsSealNumber();
                if (map.containsKey(container.getContainerNumber()))
                    commonContainers.add(shipmentContainer);
            }
        }
        model.setCommonContainers(commonContainers);

        //setting vessel
        List<BookingCarriageModel> bookingCarriages = model.getShipment().getBookingCarriagesList();
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
                    Arrays.asList("Mmsi"),
                    "=",
                    vessel
            );
            CommonV1ListRequest vesselRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(vesselCriteria).build();
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
        populateShipmentFields(model.getShipment(), false, dictionary);
        populateUserFields(model.getUser(), dictionary);
        populateConsolidationFields(model.getConsolidation(), dictionary);
        if (model.getLoadingPort() != null) {
            var loadingPort = model.getLoadingPort();
            dictionary.put(ReportConstants.LOADING_PORT_NAME, loadingPort.getName());
            dictionary.put(ReportConstants.LOADING_PORT_COUNTRY, loadingPort.getCountry());
            dictionary.put(ReportConstants.LOADING_PORT_CODE, loadingPort.getLocCode());
            dictionary.put(ReportConstants.LOADING_PORT_COUNTRY_NAME, loadingPort.getCountry().toUpperCase());
        }
        if (model.getDischargePort() != null) {
            var dischargePort = model.getDischargePort();
            dictionary.put(ReportConstants.DISCHARGE_PORT_NAME, dischargePort.getName());
            dictionary.put(ReportConstants.DISCHARGE_PORT_COUNTRY, dischargePort.getCountry());
            dictionary.put(ReportConstants.DISCHARGE_PORT_CODE, dischargePort.getLocCode());
            dictionary.put(ReportConstants.DISCHARGE_PORT_COUNTRY_NAME, dischargePort.getCountry().toUpperCase());
        }
        dictionary.put(ReportConstants.CONTAINER_COUNT_BY_CODE, getCountByContainerTypeCode(model.getCommonContainers()));

        if (model.getConsolContainers() != null && !model.getConsolContainers().isEmpty()) {
            List<Map<String, Object>> containerSummary = model.getConsolContainers().stream()
                    .sorted(Comparator.comparing(ContainerModel::getContainerCode))
                    .collect(Collectors.groupingBy(ContainerModel::getContainerCode, Collectors.counting()))
                    .entrySet()
                    .stream()
                    .map(entry -> {
                        Map<String, Object> map = new HashMap<>();
                        map.put("key", entry.getKey());
                        map.put("ContCount", entry.getValue());
                        map.put("CountDesc", entry.getKey() + " * " + entry.getValue());
                        map.put("ContainerSummary", map.get("CountDesc"));
                        return map;
                    })
                    .collect(Collectors.toList());
            dictionary.put(ReportConstants.CONSOL_CONTAINERSUMMARY, containerSummary);
        }

        dictionary.put(ReportConstants.TOTAL_PACKS, getTotalPacks(model.getShipment()));
        String unitCode = getTotalPackUnitCode(model.getShipmentList());
        dictionary.put(ReportConstants.TOTAL_PACK_UNIT_CODE, unitCode);
        dictionary.put(ReportConstants.TOTAL_PACK_UNIT_DESCRIPTION, masterListDescriptionPacksUnit(unitCode));

        dictionary.put(ReportConstants.CONSOL_CONTAINERS, model.getConsolContainers());
        dictionary.put(ReportConstants.CONSOL_CARRIER, model.getCarrier().getItemDescription());
        dictionary.put(ReportConstants.CARRIER_CONTACT_PERSON, model.getCarrier().getCarrierContactPerson().toUpperCase());
        dictionary.put(ReportConstants.CARRIER_NAME, model.getCarrier().getItemDescription().toUpperCase());
        dictionary.put(ReportConstants.SHIPMENT_AND_CONTAINER, model.getShipmentAndContainer());
        if (model.getShipmentAndContainer().size() > 0) {
            dictionary.put(ReportConstants.CONSOL_DESCRIPTION_ENABLED, false);
            dictionary.put(ReportConstants.SHIPMENT_HASCONTAINERS, true);
        } else {
            dictionary.put(ReportConstants.CONSOL_DESCRIPTION_ENABLED, true);
            dictionary.put(ReportConstants.SHIPMENT_HASCONTAINERS, false);
        }


        String jsonContainer = jsonHelper.convertToJson(dictionary.get(ReportConstants.SHIPMENT_AND_CONTAINER));

        List<Map<String, Object>> valuesContainer = jsonHelper.readFromJson(jsonContainer, List.class);

        valuesContainer.forEach(v -> {
            v.put(ReportConstants.WEIGHT, addCommas(v.get(ReportConstants.WEIGHT).toString()));
            v.put(ReportConstants.VOLUME, addCommas(v.get(ReportConstants.VOLUME).toString()));
            v.put(ReportConstants.PACKS, addCommas(v.get(ReportConstants.PACKS).toString()));
            v.put(ReportConstants.SHIPMENT_PACKS_UNIT_DESC, getMasterListData(MasterDataType.PACKS_UNIT, model.getShipment().getPacksUnit().toUpperCase()));

        });

        dictionary.put(ReportConstants.SHIPMENT_AND_CONTAINER, valuesContainer);
        dictionary.put(ReportConstants.SERVICE_MODE_DESCRIPTION,
                model.getServiceMode().toUpperCase());
        dictionary.put(ReportConstants.SERVICE_MODE, model.getShipment().getServiceType().toUpperCase());

        List<String> shipmentIds = model.getShipmentList().stream()
                .map(i -> i.getId().toString())
                .collect(Collectors.toList());
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
