package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.CargoManifestModel;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.*;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.dto.request.awb.AwbCargoInfo;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferVessels;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.core.type.TypeReference;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.*;

@Component
@Slf4j
public class CargoManifestReport extends IReport{

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private CommonUtils commonUtils;

    @Override
    public Map<String, Object> getData(Long id) {
        CargoManifestModel cargoManifestModel = (CargoManifestModel) getDocumentModel(id);
        return populateDictionary(cargoManifestModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        CargoManifestModel cargoManifestModel = new CargoManifestModel();
        cargoManifestModel.shipmentDetails = getShipment(id);
        validateAirAndOceanDGCheck(cargoManifestModel.shipmentDetails); // check
        cargoManifestModel.tenantDetails = getTenant();
        cargoManifestModel.usersDto = UserContext.getUser();
        cargoManifestModel.awb = getHawb(id);
        cargoManifestModel.shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        return cargoManifestModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        CargoManifestModel cargoManifestModel = (CargoManifestModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();
        addTenantDetails(dictionary, cargoManifestModel.tenantDetails);

        List<String> consigner = getPartyDetails(cargoManifestModel.shipmentDetails.getConsigner(), cargoManifestModel);

        List<String> consignee = getPartyDetails(cargoManifestModel.shipmentDetails.getConsignee(), cargoManifestModel);

        List<String> notify = getPartyDetails(cargoManifestModel.shipmentDetails.getAdditionalDetails().getNotifyParty(), cargoManifestModel);

        List<String> tenantsDataList = getListOfStrings(cargoManifestModel.tenantDetails.tenantName, cargoManifestModel.tenantDetails.address1, cargoManifestModel.tenantDetails.address2,
                cargoManifestModel.tenantDetails.city, cargoManifestModel.tenantDetails.state, cargoManifestModel.tenantDetails.zipPostCode, cargoManifestModel.tenantDetails.country,
                cargoManifestModel.tenantDetails.email, cargoManifestModel.tenantDetails.websiteUrl, cargoManifestModel.tenantDetails.phone);
        dictionary.put(ReportConstants.TENANT, tenantsDataList);
        dictionary.put(ReportConstants.CONSIGNOR, consigner);
        dictionary.put(ReportConstants.CONSIGNEE_ADDRESS, consignee);
        dictionary.put(ReportConstants.NOTIFY_PARTY, notify);
        dictionary.put(ReportConstants.NOTIFY_PARTY_FREETEXT, notify);
        dictionary.put(ReportConstants.CONSIGNEE_FREETEXT, consignee);
        dictionary.put(ReportConstants.CONSIGNER_FREETEXT, consigner);
        dictionary.put(ReportConstants.MAWB_NO, cargoManifestModel.shipmentDetails.getMasterBill());
        dictionary.put(ReportConstants.HAWB_NO, cargoManifestModel.shipmentDetails.getHouseBill());
        dictionary.put(ReportConstants.SHIPMENT_NO, cargoManifestModel.shipmentDetails.getShipmentId());
        Map<String, UnlocationsResponse> unlocationsMap = new HashMap<>();
        Set<String> locCodes = new HashSet<>();
        if(cargoManifestModel.shipmentDetails.getCarrierDetails().getOrigin() != null) {
            locCodes.add(cargoManifestModel.shipmentDetails.getCarrierDetails().getOrigin());
        }
        dictionary.put(ReportConstants.POL, getPortDetails(cargoManifestModel.shipmentDetails.getCarrierDetails().getOriginPort()));
        dictionary.put(ReportConstants.POD, getPortDetails(cargoManifestModel.shipmentDetails.getCarrierDetails().getDestinationPort()));
        dictionary.put(ReportConstants.FPOD, getPortDetails(cargoManifestModel.shipmentDetails.getCarrierDetails().getDestination()));
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String tsDateTimeFormat = v1TenantSettingsResponse.getDPWDateFormat();
        dictionary.put(ReportConstants.CURRENT_DATE, convertToDPWDateFormat(LocalDateTime.now(), tsDateTimeFormat, v1TenantSettingsResponse));
        if(cargoManifestModel.shipmentDetails.getCarrierDetails().getEtd() != null) {
            dictionary.put(ReportConstants.ETD_CAPS, convertToDPWDateFormat(cargoManifestModel.shipmentDetails.getCarrierDetails().getEtd(), tsDateTimeFormat, v1TenantSettingsResponse));
        }
        if(cargoManifestModel.shipmentDetails.getCarrierDetails().getEta() != null) {
            dictionary.put(ReportConstants.ETA_CAPS, convertToDPWDateFormat(cargoManifestModel.shipmentDetails.getCarrierDetails().getEta(), tsDateTimeFormat, v1TenantSettingsResponse));
        }
        dictionary.put(ReportConstants.FLIGHT_NAME, cargoManifestModel.shipmentDetails.getCarrierDetails().getShippingLine());
        dictionary.put(ReportConstants.FLIGHT_NUMBER, cargoManifestModel.shipmentDetails.getCarrierDetails().getFlightNumber());
        dictionary.put(ReportConstants.PP_CC, cargoManifestModel.shipmentDetails.getPaymentTerms());
        dictionary.put(ReportConstants.BOOKING_NO, cargoManifestModel.shipmentDetails.getBookingNumber());
        processShipmentPackingList(cargoManifestModel, v1TenantSettingsResponse, dictionary);
        dictionary.put(ReportConstants.CMS_REMARKS, cargoManifestModel.shipmentDetails.getAdditionalTerms());
        dictionary.put(ReportConstants.USER_EMAIL, cargoManifestModel.usersDto.Email);
        dictionary.put(ReportConstants.DATE_TIME, LocalDateTime.now().format(DateTimeFormatter.ofPattern("dd/MMM/y hh:mm a")));
        List<String> unlocoRequests = this.createUnLocoRequestFromShipmentModel(cargoManifestModel.shipmentDetails);
        locCodes.addAll(unlocoRequests);
        Map<String, EntityTransferUnLocations> entityTransferUnLocationsMap = masterDataUtils.getLocationDataFromCache(locCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
        for (Map.Entry<String, EntityTransferUnLocations> entry : entityTransferUnLocationsMap.entrySet()) {
            String key = entry.getKey();
            UnlocationsResponse value = jsonHelper.convertValue(entry.getValue(), UnlocationsResponse.class);
            unlocationsMap.put(key, value);
        }
        UnlocationsResponse origin = unlocationsMap.get(cargoManifestModel.shipmentDetails.getCarrierDetails().getOrigin());
        if(origin != null)
            dictionary.put(ReportConstants.POR, origin.getNameWoDiacritics());
        try {dictionary.put(ReportConstants.POR_IN_CAPS, unlocationsMap.get(cargoManifestModel.shipmentDetails.getCarrierDetails().getOrigin()).getName().toUpperCase());} catch (Exception ignored) { log.info(Constants.IGNORED_ERROR_MSG); }
        try {dictionary.put(ReportConstants.POL_IN_CAPS, unlocationsMap.get(cargoManifestModel.shipmentDetails.getCarrierDetails().getOriginPort()).getPortName().toUpperCase());} catch (Exception ignored) { log.info(Constants.IGNORED_ERROR_MSG); }
        try {dictionary.put(ReportConstants.FPOD_IN_CAPS, unlocationsMap.get(cargoManifestModel.shipmentDetails.getCarrierDetails().getDestinationPort()).getPortName().toUpperCase());} catch (Exception ignored) { log.info(Constants.IGNORED_ERROR_MSG); }
        try {dictionary.put(ReportConstants.POD_IN_CAPS, unlocationsMap.get(cargoManifestModel.shipmentDetails.getCarrierDetails().getDestination()).getName().toUpperCase());} catch (Exception ignored) { log.info(Constants.IGNORED_ERROR_MSG); }

        try {dictionary.put(ReportConstants.POR_COUNTRY_NAME_IN_CAPS, unlocationsMap.get(cargoManifestModel.shipmentDetails.getCarrierDetails().getOrigin()).getCountry().toUpperCase());} catch (Exception ignored) { log.info(Constants.IGNORED_ERROR_MSG); }
        try {dictionary.put(ReportConstants.POL_COUNTRY_NAME_IN_CAPS, unlocationsMap.get(cargoManifestModel.shipmentDetails.getCarrierDetails().getOriginPort()).getCountry().toUpperCase());} catch (Exception ignored) { log.info(Constants.IGNORED_ERROR_MSG); }
        try {dictionary.put(ReportConstants.POD_COUNTRY_NAME_IN_CAPS, unlocationsMap.get(cargoManifestModel.shipmentDetails.getCarrierDetails().getDestinationPort()).getCountry().toUpperCase());} catch (Exception ignored) { log.info(Constants.IGNORED_ERROR_MSG); }
        try {dictionary.put(ReportConstants.FPOD_COUNTRY_NAME_IN_CAPS, unlocationsMap.get(cargoManifestModel.shipmentDetails.getCarrierDetails().getDestination()).getCountry().toUpperCase());} catch (Exception ignored) { log.info(Constants.IGNORED_ERROR_MSG); }

        processShipmentPaymentTerms(cargoManifestModel, dictionary);
        processShipmentPackUnit(cargoManifestModel, dictionary);
        processShipmentShippingLine(cargoManifestModel, dictionary);
        processShipmentBookingCarriageList(cargoManifestModel, dictionary, tsDateTimeFormat, v1TenantSettingsResponse, unlocationsMap);
        dictionary.put(ReportConstants.CONTAINER_TYPE, cargoManifestModel.shipmentDetails.getShipmentType());
        processShipmentContainersList(cargoManifestModel, dictionary, v1TenantSettingsResponse);

        processCargoManifestAwb(cargoManifestModel, dictionary);
        populateRaKcData(dictionary, cargoManifestModel.shipmentDetails);

        processShipmentPackingList(cargoManifestModel, dictionary);
        if(cargoManifestModel.shipmentDetails.getAdditionalDetails() != null) {
            dictionary.put(NOTIFY_PARTY, ReportHelper.getOrgAddressDetails(cargoManifestModel.shipmentDetails.getAdditionalDetails().getNotifyParty()));
        }

        processConsolidation(cargoManifestModel, dictionary, unlocationsMap);

        dictionary.put(INSERT_DATE, convertToDPWDateFormat(LocalDateTime.now(), tsDateTimeFormat, v1TenantSettingsResponse));
        dictionary.put(TOTAL_WEIGHT, convertToWeightNumberFormat(cargoManifestModel.shipmentDetails.getWeight(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.PWEIGHT_UNIT, cargoManifestModel.shipmentDetails.getWeightUnit());
        dictionary.put(TOTAL_VOLUME, convertToVolumeNumberFormat(cargoManifestModel.shipmentDetails.getVolume(), v1TenantSettingsResponse));
        dictionary.put(PVOLUME_UNIT, cargoManifestModel.shipmentDetails.getVolumeUnit());
        dictionary.put(CHARGEABLE, convertToWeightNumberFormat(cargoManifestModel.shipmentDetails.getChargable(), v1TenantSettingsResponse));
        dictionary.put(PCHARGE_UNIT, cargoManifestModel.shipmentDetails.getChargeableUnit());
        dictionary.put(TOTAL_PACKAGES, cargoManifestModel.shipmentDetails.getNoOfPacks());
        dictionary.put(PACKS_UNIT, cargoManifestModel.shipmentDetails.getPacksUnit());
        dictionary.put(CARRIER_BOOKING_REF, cargoManifestModel.shipmentDetails.getBookingNumber());
        dictionary.put(SERVICE_LEVEL, cargoManifestModel.shipmentDetails.getServiceType());

        processShipmentRoutingList(cargoManifestModel, dictionary);

        processReferenceNumbersList(cargoManifestModel, dictionary);
        return dictionary;
    }

    private void processCargoManifestAwb(CargoManifestModel cargoManifestModel, Map<String, Object> dictionary) {
        if(cargoManifestModel.awb != null) {
            AwbCargoInfo cargoInfoRows = cargoManifestModel.awb.getAwbCargoInfo();
            dictionary.put(ReportConstants.SCI, cargoInfoRows.getSci());
            dictionary.put(RA_CSD, geteCSDInfo(cargoManifestModel.awb));
            dictionary.put(ORIGINAL_PRINT_DATE, getPrintOriginalDate(cargoManifestModel.awb));
            dictionary.put(USER_INITIALS, Optional.ofNullable(cargoInfoRows.getUserInitials()).map(StringUtility::toUpperCase).orElse(Constants.EMPTY_STRING));
        }
    }

    private void processShipmentShippingLine(CargoManifestModel cargoManifestModel, Map<String, Object> dictionary) {
        try {
            if(!CommonUtils.isStringNullOrEmpty(cargoManifestModel.shipmentDetails.getCarrierDetails().getShippingLine())) {
                Set<String> carrierSet = new HashSet<>();
                carrierSet.add(cargoManifestModel.shipmentDetails.getCarrierDetails().getShippingLine());
                Map<String, EntityTransferCarrier> entityTransferCarrierMap = masterDataUtils.getCarrierDataFromCache(carrierSet);
                dictionary.put(ReportConstants.FLIGHT_IATA_CODE, entityTransferCarrierMap.get(cargoManifestModel.shipmentDetails.getCarrierDetails().getShippingLine()).IATACode);
            }
        } catch (Exception ignored) {
            log.info(Constants.IGNORED_ERROR_MSG);
        }
    }

    private void processShipmentPaymentTerms(CargoManifestModel cargoManifestModel, Map<String, Object> dictionary) {
        if(!CommonUtils.isStringNullOrEmpty(cargoManifestModel.shipmentDetails.getPaymentTerms())) {
            MasterData paymentTerms = getMasterListData(MasterDataType.PAYMENT, cargoManifestModel.shipmentDetails.getPaymentTerms());
            try {
                dictionary.put(ReportConstants.PAYMENT_TERMS_DESCRIPTION, paymentTerms.getItemDescription());
            } catch (Exception ignored) {
                log.info(Constants.IGNORED_ERROR_MSG);
            }
            try {
                dictionary.put(ReportConstants.PAYMENT_TERMS, cargoManifestModel.shipmentDetails.getPaymentTerms());
            } catch (Exception ignored) {
                log.info(Constants.IGNORED_ERROR_MSG);
            }
        }
    }

    private void processShipmentPackUnit(CargoManifestModel cargoManifestModel, Map<String, Object> dictionary) {
        if(!CommonUtils.isStringNullOrEmpty(cargoManifestModel.shipmentDetails.getPacksUnit())) {
            MasterData packsUnitDesc = getMasterListData(MasterDataType.PACKS_UNIT, cargoManifestModel.shipmentDetails.getPacksUnit());
            String packsUnit = null;
            try {
                packsUnit = packsUnitDesc.getItemDescription();
            } catch (Exception ignored) {
                log.info(Constants.IGNORED_ERROR_MSG);
            }
            if(CommonUtils.isStringNullOrEmpty(packsUnit))
                packsUnit = cargoManifestModel.shipmentDetails.getPacksUnit();
            dictionary.put(ReportConstants.PACKS_UNIT_DESCRIPTION, Constants.MPK.equals(packsUnit) ? Constants.PACKAGES : packsUnit);
        }
    }

    private void processShipmentPackingList(CargoManifestModel cargoManifestModel, Map<String, Object> dictionary) {
        if(!listIsNullOrEmpty(cargoManifestModel.shipmentDetails.getPackingList())) {
            getPackingDetails(cargoManifestModel.shipmentDetails, dictionary);
            dictionary.put(HAS_PACK_DETAILS, true);
            var hazardousCheck = cargoManifestModel.shipmentDetails.getPackingList().stream().anyMatch(x -> !Objects.isNull(x.getHazardous()) && x.getHazardous());
            var temperatureCheck = cargoManifestModel.shipmentDetails.getPackingList().stream().anyMatch(x -> !Objects.isNull(x.getIsTemperatureControlled()) && x.getIsTemperatureControlled());
            dictionary.put(HAS_DANGEROUS_GOODS, hazardousCheck);
            dictionary.put(HAS_TEMPERATURE_DETAILS, temperatureCheck);
        } else {
            dictionary.put(HAS_PACK_DETAILS, false);
        }
    }

    private void processReferenceNumbersList(CargoManifestModel cargoManifestModel, Map<String, Object> dictionary) {
        if (cargoManifestModel.shipmentDetails.getReferenceNumbersList() != null && !cargoManifestModel.shipmentDetails.getReferenceNumbersList().isEmpty()) {
            Optional<ReferenceNumbersModel> refNumberModel = cargoManifestModel.shipmentDetails.getReferenceNumbersList().stream()
                    .filter(i -> ReportConstants.CEN.equals(i.getType()))
                    .reduce((first, second) -> second);
            refNumberModel.ifPresent(referenceNumbersModel -> dictionary.put(CUSTOMS_ENTRY_NUMBER, referenceNumbersModel.getReferenceNumber()));
        }
    }

    private List<String> getPartyDetails(PartiesModel shipmentDetails, CargoManifestModel cargoManifestModel) {
        List<String> consigner = new ArrayList<>();
        if (shipmentDetails != null) {
            consigner = getOrgAddressWithPhoneEmail(shipmentDetails);
            if (shipmentDetails.getOrgData() != null) {
                Map<String, Object> partyOrg = shipmentDetails.getOrgData();
                if (!Boolean.TRUE.equals(cargoManifestModel.shipmentSettingsDetails.getDisableBlPartiesName()) && getValueFromMap(partyOrg, ReportConstants.FULL_NAME) != null) {
                    consigner.add(0, getValueFromMap(partyOrg, ReportConstants.FULL_NAME));
                }
            }
        }
        return consigner;
    }

    private void processShipmentRoutingList(CargoManifestModel cargoManifestModel, Map<String, Object> dictionary) {
        if (cargoManifestModel.shipmentDetails.getRoutingsList() != null && !cargoManifestModel.shipmentDetails.getRoutingsList().isEmpty()) {
            List<RoutingsModel> mainCarriageRouts = cargoManifestModel.shipmentDetails.getRoutingsList().stream()
                    .filter(i -> RoutingCarriage.MAIN_CARRIAGE.equals(i.getCarriage()))
                    .toList();
            if (!mainCarriageRouts.isEmpty()) {
                List<Map<String, Object>> mainCarriageRoutsList = new ArrayList<>();
                Set<String> vesselGuids = new HashSet<>();
                mainCarriageRouts.stream().filter(e -> !CommonUtils.isStringNullOrEmpty(e.getVesselName())).forEach(e -> vesselGuids.add(e.getVesselName()));
                Map<String, EntityTransferVessels> vesselsMap = masterDataUtils.fetchInBulkVessels(vesselGuids);
                for (RoutingsModel route : mainCarriageRouts) {
                    Map<String, Object> routeMap = new HashMap<>();
                    routeMap.put(MODE, route.getMode());
                    if(!CommonUtils.isStringNullOrEmpty(route.getVesselName()) && vesselsMap.containsKey(route.getVesselName()))
                        routeMap.put(VESSEL_NAME, vesselsMap.get(route.getVesselName()).getName());
                    routeMap.put(VOYAGE, route.getVoyage());
                    routeMap.put(CARRIER, route.getCarrier());
                    routeMap.put(POLCODE, route.getPol());
                    routeMap.put(PODCODE, route.getPod());
                    routeMap.put(ETD_FOR_PRINT, convertToDPWDateFormat(route.getEtd()));
                    routeMap.put(ETA_FOR_PRINT, convertToDPWDateFormat(route.getEta()));
                    mainCarriageRoutsList.add(routeMap);
                }
                dictionary.put(ROUTINGS, mainCarriageRoutsList);
            }
        }
    }

    private void processConsolidation(CargoManifestModel cargoManifestModel, Map<String, Object> dictionary, Map<String, UnlocationsResponse> unlocationsMap) {
        if (cargoManifestModel.shipmentDetails.getConsolidationList() != null && !cargoManifestModel.shipmentDetails.getConsolidationList().isEmpty()) {
            ConsolidationModel consol = cargoManifestModel.shipmentDetails.getConsolidationList().get(0);
            dictionary.put(CONSOLIDATION_NUMBER, consol.getConsolidationNumber());
            dictionary.put(AGENT_REFERENCE, consol.getAgentReference());
            if(Constants.IMP.equalsIgnoreCase(cargoManifestModel.shipmentDetails.getDirection())) {
                var ctoAddress = consol.getArrivalDetails() == null ? new ArrayList<>(): ReportHelper.getOrgAddress(consol.getArrivalDetails().getCTOId());
                dictionary.put(CTO_ADDRESS, ctoAddress);
                Set<String> arrivalRequest = new HashSet<>();
                processConsoleArrivalDetails(dictionary, unlocationsMap, consol, arrivalRequest);
            } else {
                var ctoAddress = consol.getDepartureDetails() == null ? new ArrayList<>(): ReportHelper.getOrgAddress(consol.getDepartureDetails().getCTOId());
                dictionary.put(CTO_ADDRESS, ctoAddress);
                Set<String> departureRequest = new HashSet<>();
                processConsoleDepartureDetails(dictionary, unlocationsMap, consol, departureRequest);
            }
        }
    }

    private void processConsoleArrivalDetails(Map<String, Object> dictionary, Map<String, UnlocationsResponse> unlocationsMap, ConsolidationModel consol, Set<String> arrivalRequest) {
        if(consol.getArrivalDetails()!=null){
            arrivalRequest.add(consol.getArrivalDetails().getLastForeignPort());
            Map<String, EntityTransferUnLocations> arrivalEntityTransferUnlocMap = masterDataUtils.getLocationDataFromCache(arrivalRequest, EntityTransferConstants.LOCATION_SERVICE_GUID);
            for (Map.Entry<String, EntityTransferUnLocations> entry : arrivalEntityTransferUnlocMap.entrySet()) {
                String key = entry.getKey();
                UnlocationsResponse value = jsonHelper.convertValue(entry.getValue(), UnlocationsResponse.class);
                unlocationsMap.put(key, value);
            }
            UnlocationsResponse arrival = unlocationsMap.get(consol.getArrivalDetails().getLastForeignPort());
            if (arrival != null)
                dictionary.put(LAST_FOREIGN_PORT_NAME, arrival.getLocCode());
        }
    }

    private void processConsoleDepartureDetails(Map<String, Object> dictionary, Map<String, UnlocationsResponse> unlocationsMap, ConsolidationModel consol, Set<String> departureRequest) {
        if(consol.getDepartureDetails()!=null){
            departureRequest.add(consol.getDepartureDetails().getLastForeignPort());
            Map<String, EntityTransferUnLocations> departureEntityTransferUnlocMap = masterDataUtils.getLocationDataFromCache(departureRequest, EntityTransferConstants.LOCATION_SERVICE_GUID);
            for (Map.Entry<String, EntityTransferUnLocations> entry : departureEntityTransferUnlocMap.entrySet()) {
                String key = entry.getKey();
                UnlocationsResponse value = jsonHelper.convertValue(entry.getValue(), UnlocationsResponse.class);
                unlocationsMap.put(key, value);
            }
            UnlocationsResponse departure = unlocationsMap.get(consol.getDepartureDetails().getLastForeignPort());
            if (departure != null)
                dictionary.put(LAST_FOREIGN_PORT_NAME, departure.getLocCode());
        }
    }


    private void processShipmentContainersList(CargoManifestModel cargoManifestModel, Map<String, Object> dictionary, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if (cargoManifestModel.shipmentDetails.getContainersList() == null || cargoManifestModel.shipmentDetails.getContainersList().isEmpty()) {
            return; // Early return if the list is empty or null
        }

        List<ContainerModel> containers = cargoManifestModel.shipmentDetails.getContainersList();
        List<ShipmentContainers> shipmentContainersList = new ArrayList<>();
        BigDecimal totalGrossWeight = BigDecimal.ZERO;
        BigDecimal totalGrossVolume = BigDecimal.ZERO;
        long totalContainerCount = 0;
        long totalPacks = 0;

        // Process each container and calculate totals
        for (ContainerModel item : containers) {
            shipmentContainersList.add(getShipmentContainer(item));

            if (item.getGrossWeight() != null) {
                totalGrossWeight = totalGrossWeight.add(item.getGrossWeight());
            }
            if (item.getGrossVolume() != null) {
                totalGrossVolume = totalGrossVolume.add(item.getGrossVolume());
            }
            if (item.getContainerCount() != null) {
                totalContainerCount += item.getContainerCount();
            }
            if (!CommonUtils.isStringNullOrEmpty(item.getPacks())) {
                totalPacks += Long.parseLong(item.getPacks());
            }
        }

        // Convert ShipmentContainers to Map and process values
        List<Map<String, Object>> valuesContainer = new ArrayList<>();
        for (ShipmentContainers shipmentContainers : shipmentContainersList) {
            valuesContainer.add(jsonHelper.convertValue(shipmentContainers, new TypeReference<>() {}));
        }
        processValuesContainer(v1TenantSettingsResponse, valuesContainer);

        // Update dictionary with processed values and totals
        dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, valuesContainer);
        dictionary.put(COMMON_CONTAINERS, valuesContainer);
        dictionary.put(ReportConstants.TOTAL_CNTR_WEIGHT, convertToWeightNumberFormat(totalGrossWeight, v1TenantSettingsResponse));
        dictionary.put(ReportConstants.TOTAL_CNTR_VOLUME, convertToVolumeNumberFormat(totalGrossVolume, v1TenantSettingsResponse));
        dictionary.put(ReportConstants.TOTAL_CNTR_COUNT, addCommaWithoutDecimal(new BigDecimal(totalContainerCount)));
        dictionary.put(ReportConstants.TOTAL_CNTR_PACKS, addCommaWithoutDecimal(new BigDecimal(totalPacks)));
    }

    private void processValuesContainer(V1TenantSettingsResponse v1TenantSettingsResponse, List<Map<String, Object>> valuesContainer) {
        for (Map<String, Object> v : valuesContainer) {
            updateValue(v, ReportConstants.GROSS_VOLUME, convertToVolumeNumberFormat(v.get(ReportConstants.GROSS_VOLUME), v1TenantSettingsResponse));
            updateValue(v, ReportConstants.GROSS_WEIGHT, convertToWeightNumberFormat(v.get(ReportConstants.GROSS_WEIGHT), v1TenantSettingsResponse));
            updateValue(v, ReportConstants.SHIPMENT_PACKS, addCommaWithoutDecimal(new BigDecimal(v.get(ReportConstants.SHIPMENT_PACKS).toString())));
            updateValue(v, ReportConstants.TARE_WEIGHT, convertToWeightNumberFormat(v.get(ReportConstants.TARE_WEIGHT), v1TenantSettingsResponse));
            updateValue(v, ReportConstants.VGM_WEIGHT, convertToWeightNumberFormat(v.get(ReportConstants.VGM_WEIGHT), v1TenantSettingsResponse));

            if (v.containsKey(CONTAINER_TYPE_CODE)) {
                v.put(CONTAINER_TYPE_DESCRIPTION, v.get(CONTAINER_TYPE_CODE));
            }
        }
    }

    private void updateValue(Map<String, Object> map, String key, Object newValue) {
        if (map.containsKey(key) && map.get(key) != null) {
            map.put(key, newValue);
        }
    }

    private void processShipmentBookingCarriageList(CargoManifestModel cargoManifestModel, Map<String, Object> dictionary, String tsDateTimeFormat, V1TenantSettingsResponse v1TenantSettingsResponse, Map<String, UnlocationsResponse> unlocationsMap) {
        if(cargoManifestModel.shipmentDetails.getBookingCarriagesList() != null && !cargoManifestModel.shipmentDetails.getBookingCarriagesList().isEmpty()) {
            Set<String> unlocoStrings = new HashSet<>();
            for (BookingCarriageModel bookingCarriageModel : cargoManifestModel.shipmentDetails.getBookingCarriagesList()) {
                if (bookingCarriageModel.getCarriageType() != null && (bookingCarriageModel.getCarriageType().equals(Constants.PRE_CARRIAGE) || bookingCarriageModel.getCarriageType().equals(Constants.MAIN))) {
                    dictionary.put(bookingCarriageModel.getCarriageType() + ReportConstants.VESSEL, bookingCarriageModel.getVessel());
                    dictionary.put(bookingCarriageModel.getCarriageType() + ReportConstants.VOYAGE, bookingCarriageModel.getVoyage());
                    dictionary.put(bookingCarriageModel.getCarriageType() + ReportConstants.ETD_CAPS, convertToDPWDateFormat(bookingCarriageModel.getEtd(), tsDateTimeFormat, v1TenantSettingsResponse));
                    dictionary.put(bookingCarriageModel.getCarriageType() + ReportConstants.ETA_CAPS, convertToDPWDateFormat(bookingCarriageModel.getEta(), tsDateTimeFormat, v1TenantSettingsResponse));
                    unlocoStrings.add(bookingCarriageModel.getPortOfLoading());
                    unlocoStrings.add(bookingCarriageModel.getPortOfDischarge());
                    Map<String, EntityTransferUnLocations> entityUnLocationsMap = masterDataUtils.getLocationDataFromCache(unlocoStrings, EntityTransferConstants.LOCATION_SERVICE_GUID);
                    for (Map.Entry<String, EntityTransferUnLocations> entry : entityUnLocationsMap.entrySet()) {
                        String key = entry.getKey();
                        UnlocationsResponse value = jsonHelper.convertValue(entry.getValue(), UnlocationsResponse.class);
                        unlocationsMap.put(key, value);
                    }
                    processPolPod(dictionary, unlocationsMap, bookingCarriageModel);
                }
            }
        }
    }

    private void processPolPod(Map<String, Object> dictionary, Map<String, UnlocationsResponse> unlocationsMap, BookingCarriageModel bookingCarriageModel) {
        UnlocationsResponse pol = unlocationsMap.get(bookingCarriageModel.getPortOfLoading());
        UnlocationsResponse pod = unlocationsMap.get(bookingCarriageModel.getPortOfDischarge());
        if (pol != null) {
            dictionary.put(bookingCarriageModel.getCarriageType() + ReportConstants.PLACE_OF_LOAD_COUNTRY, pol.getCountry());
            dictionary.put(bookingCarriageModel.getCarriageType() + ReportConstants.PLACE_OF_LOAD_PORT, pol.getPortName());
            dictionary.put(bookingCarriageModel.getCarriageType() + ReportConstants.PLACE_OF_LOAD_CODE, pol.getLocCode());
        }
        if (pod != null) {
            dictionary.put(bookingCarriageModel.getCarriageType() + ReportConstants.PLACE_OF_DISCHARGE_COUNTRY, pod.getCountry());
            dictionary.put(bookingCarriageModel.getCarriageType() + ReportConstants.PLACE_OF_DISCHARGE_PORT, pod.getPortName());
            dictionary.put(bookingCarriageModel.getCarriageType() + ReportConstants.PLACE_OF_DISCHARGE_CODE, pod.getLocCode());
        }
    }

    private void processShipmentPackingList(CargoManifestModel cargoManifestModel, V1TenantSettingsResponse v1TenantSettingsResponse, Map<String, Object> dictionary) {
        if(cargoManifestModel.shipmentDetails.getPackingList() != null && !cargoManifestModel.shipmentDetails.getPackingList().isEmpty()) {
            var request = cargoManifestModel.shipmentDetails.getPackingList().stream().map(PackingModel::getCommodity).filter(StringUtility::isNotEmpty).toList();
            var v1DataMap = masterDataUtils.fetchInBulkCommodityTypes(request);
            List<Map<String, Object>> packDictionary = new ArrayList<>();
            for (PackingModel pack : cargoManifestModel.shipmentDetails.getPackingList()) {
                var map = jsonHelper.convertJsonToMap(jsonHelper.convertToJson(pack));
                map.put(ReportConstants.WEIGHT, convertToWeightNumberFormat(map.get(ReportConstants.WEIGHT), v1TenantSettingsResponse));
                map.put(ReportConstants.NET_WEIGHT, convertToWeightNumberFormat(map.get(ReportConstants.NET_WEIGHT), v1TenantSettingsResponse));
                map.put(ReportConstants.VOLUME_WEIGHT, convertToWeightNumberFormat(map.get(ReportConstants.VOLUME_WEIGHT), v1TenantSettingsResponse));
                map.put(ReportConstants.VOLUME, convertToVolumeNumberFormat(map.get(ReportConstants.VOLUME), v1TenantSettingsResponse));
                if (v1DataMap.containsKey(pack.getCommodity()))
                    map.put(ReportConstants.COMMODITY_NAME, v1DataMap.get(pack.getCommodity()).getDescription());
                packDictionary.add(map);
            }
            packDictionary.forEach(this::jsonDateFormat);
            dictionary.put(ReportConstants.ITEMS, packDictionary);
        }
    }

}
