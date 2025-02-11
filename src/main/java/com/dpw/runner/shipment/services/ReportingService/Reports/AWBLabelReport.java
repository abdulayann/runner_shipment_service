package com.dpw.runner.shipment.services.ReportingService.Reports;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.AIR;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.AIRLINE_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CARRIER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGEABLE_WEIGHT_UNIT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOL_CHARGEABLE_WEIGHT_AND_UNIT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOL_DESTINATION_AIRPORT_CODE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOL_DESTINATION_AIRPORT_CODE_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOL_FIRST_LEG_DESTINATION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOL_ORIGIN_AIRPORT_CODE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOL_ORIGIN_AIRPORT_CODE_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOL_SECOND_LEG_DESTIATION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOL_THIRD_LEG_DESTINATION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOL_TOTAL_WEIGHT_AND_UNIT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DESTINATION_PORT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DESTINATION_PORT_NAME_INCAPS_AIR;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FIRST_LEG_DESTINATION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.GROSS_WEIGHT_AND_UNIT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.HAWB_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.HAWB_NOS_PACKS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.IS_COMBI;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.MAWB_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ORIGIN_PORT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ORIGIN_PORT_NAME_INCAPS_AIR;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PACKS_UNIT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.POD_AIRPORT_CODE_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.POL_AIRPORT_CODE_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SECOND_LEG_DESTINATION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.THIRD_LEG_DESTINATION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TOTAL_CONSOL_PACKS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TOTAL_PACKS;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.AWbLabelModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PackingModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.RoutingsModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentMeasurementDetailsDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

@Component
@Slf4j
public class AWBLabelReport extends IReport{
    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private IPackingService packingService;

    public void setMawb(boolean mawb) {
        isMawb = mawb;
    }

    public void setCombi(boolean combi) {
        isCombi = combi;
    }

    public void setRemarks(String remarks) {
        this.remarks = remarks;
    }

    public boolean isMawb() {
        return isMawb;
    }

    public String getRemarks() {
        return remarks;
    }

    private boolean isCombi;
    private boolean isMawb;
    private String remarks;
    @Override
    public Map<String, Object> getData(Long id) throws RunnerException {
        AWbLabelModel awbLabelModel = (AWbLabelModel) getDocumentModel(id);
        return populateDictionary(awbLabelModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) throws RunnerException {
        AWbLabelModel awbLabelModel = new AWbLabelModel();
        if(isCombi) {
            ConsolidationDetails consolidationDetails;
            Long consoleId;
            if(isMawb) {
                consoleId = id;
                consolidationDetails = getConsolidationsById(consoleId);
            }
            else {
                ShipmentDetails shipmentDetails = getShipmentDetails(id);
                if(listIsNullOrEmpty(shipmentDetails.getConsolidationList()))
                    throw new RunnerException("Please attach the consolidation before printing Combi Labels");
                consoleId = shipmentDetails.getConsolidationList().get(0).getId();
                consolidationDetails = getConsolidationsById(consoleId);
            }
            awbLabelModel.setConsolidation(getConsolidationModel(consolidationDetails));
            awbLabelModel.setAwb(getMawb(consoleId, true));
            awbLabelModel.getConsolidation().setConsoleGrossWeightAndUnit(getConsolGrossWeightAndUnit(awbLabelModel.getConsolidation()));
            awbLabelModel.setShipmentModels(new ArrayList<>());
            if(!listIsNullOrEmpty(consolidationDetails.getShipmentsList())) {
                for (ShipmentDetails shipmentDetails: consolidationDetails.getShipmentsList()) {
                    awbLabelModel.getShipmentModels().add(getShipment(shipmentDetails));
                }
            }
            isMawb = true;
        } else {
            if(isMawb) {
                awbLabelModel.setConsolidation(getConsolidation(id));
                awbLabelModel.setAwb(getMawb(id, true));
                awbLabelModel.getConsolidation().setConsoleGrossWeightAndUnit(getConsolGrossWeightAndUnit(awbLabelModel.getConsolidation()));
            }
            else {
                awbLabelModel.shipment = getShipment(id);
                awbLabelModel.setAwb(getHawb(id));
                validateAirAndOceanDGCheck(awbLabelModel.shipment);
            }
        }
        awbLabelModel.tenant = getTenant();
        awbLabelModel.setRemarks(this.remarks);
        return awbLabelModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) throws RunnerException {
        AWbLabelModel awbLabelModel = (AWbLabelModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();
        List<String> unlocations = new ArrayList<>();

        if(awbLabelModel.shipment != null) {
            if((awbLabelModel.getShipment().getTransportMode() != null && awbLabelModel.getShipment().getTransportMode().equals(AIR)) && (awbLabelModel.getShipment().getJobType() != null && awbLabelModel.getShipment().getJobType().equals("DRT")))
                isMawb = true;
            String mawb = awbLabelModel.shipment.getMasterBill();
            V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
            populateMawb(dictionary, mawb);
            if (awbLabelModel.shipment.getCarrierDetails() != null)
                dictionary.put(ReportConstants.DESTINATION, awbLabelModel.shipment.getCarrierDetails().getDestinationPort());
            if (awbLabelModel.shipment.getInnerPacks() != null) {
                var inners = awbLabelModel.shipment.getInnerPacks().toString();
                int size = inners.length();
                if (size < 4) {
                    for (int i = 0; i < 4 - size; i++) {
                        inners = "0" + inners;
                    }
                }
                dictionary.put(ReportConstants.INNERS, GetDPWWeightVolumeFormat(BigDecimal.valueOf(Long.parseLong(inners)), 0, v1TenantSettingsResponse));
            }
            if (awbLabelModel.shipment.getNoOfPacks() != null) {
                var packs = awbLabelModel.shipment.getNoOfPacks().toString();
                int size = packs.length();
                if (size < 4) {
                    for (int i = 0; i < 4 - size; i++) {
                        packs = "0" + packs;
                    }
                }
                dictionary.put(ReportConstants.PACKS, GetDPWWeightVolumeFormat(BigDecimal.valueOf(Long.parseLong(packs)), 0, v1TenantSettingsResponse));
            }
            dictionary.put(ReportConstants.HAWB_NUMBER, awbLabelModel.shipment.getHouseBill());
            if (awbLabelModel.tenant != null) {
                ReportHelper.addTenantDetails(dictionary, awbLabelModel.tenant);
                awbLabelModel.setTenantAddress(ReportHelper.getListOfStrings(awbLabelModel.tenant.tenantName, awbLabelModel.tenant.address1,
                        awbLabelModel.tenant.address2, awbLabelModel.tenant.city, awbLabelModel.tenant.state,
                        awbLabelModel.tenant.zipPostCode, awbLabelModel.tenant.country, awbLabelModel.tenant.email,
                        awbLabelModel.tenant.websiteUrl, awbLabelModel.tenant.phone));
                if (awbLabelModel.getTenantAddress() != null)
                    dictionary.put(ReportConstants.TENANT, awbLabelModel.getTenantAddress());
            }

            if (awbLabelModel.shipment.getCarrierDetails() != null && awbLabelModel.shipment.getCarrierDetails().getDestination() != null)
                unlocations.add(awbLabelModel.shipment.getCarrierDetails().getDestination());
            if (awbLabelModel.shipment.getCarrierDetails() != null && awbLabelModel.shipment.getCarrierDetails().getDestinationPort() != null)
                unlocations.add(awbLabelModel.shipment.getCarrierDetails().getDestinationPort());
            dictionary.put(GROSS_WEIGHT_AND_UNIT, IReport.ConvertToWeightNumberFormat(awbLabelModel.shipment.getWeight(), v1TenantSettingsResponse) + " " +   convertToSingleCharWeightFormat(awbLabelModel.shipment.getWeightUnit()));
        }
        if(awbLabelModel.getConsolidation() != null){
            String shippingLine = awbLabelModel.getConsolidation().getCarrierDetails() != null ? awbLabelModel.getConsolidation().getCarrierDetails().getShippingLine() : "";
            if(!CommonUtils.IsStringNullOrEmpty(shippingLine)) {
                dictionary.put(AIRLINE_NAME, shippingLine);
            }
            if(awbLabelModel.getConsolidation().getCarrierDetails() != null && awbLabelModel.getConsolidation().getCarrierDetails().getDestination() != null)
                unlocations.add(awbLabelModel.getConsolidation().getCarrierDetails().getDestination());

            if(awbLabelModel.getConsolidation().getCarrierDetails() != null && awbLabelModel.getConsolidation().getCarrierDetails().getDestinationPort() != null)
                unlocations.add(awbLabelModel.getConsolidation().getCarrierDetails().getDestinationPort());

            String totalGrossWeightAndUnit = awbLabelModel.getConsolidation().getConsoleGrossWeightAndUnit();
            String[] parts = totalGrossWeightAndUnit.split(" ");
            String totalGrossWeight = parts[0];
            String unit = parts[1];
            dictionary.put(GROSS_WEIGHT_AND_UNIT, totalGrossWeight  + " " +   convertToSingleCharWeightFormat(unit));
        }

        if (!unlocations.isEmpty()) {
            List<UnlocationsResponse> unlocationsResponse = getUnlocationsResponses(unlocations);
            if (unlocationsResponse != null && !unlocationsResponse.isEmpty()) {
                for (var unloc : unlocationsResponse) {
                    if (awbLabelModel.getShipment() != null && Objects.equals(unloc.getLocationsReferenceGUID(), awbLabelModel.shipment.getCarrierDetails().getDestination())) {
                        dictionary.put(ReportConstants.HDEST, unloc.getName());
                    }
                    if (awbLabelModel.getShipment() != null && Objects.equals(unloc.getLocationsReferenceGUID(), awbLabelModel.shipment.getCarrierDetails().getDestinationPort())) {
                        dictionary.put(ReportConstants.DESTINATION, unloc.getLocCode());
                    }
                    if (awbLabelModel.getConsolidation() != null && Objects.equals(unloc.getLocationsReferenceGUID(), awbLabelModel.getConsolidation().getCarrierDetails().getDestination())) {
                        dictionary.put(ReportConstants.HDEST, unloc.getName());
                    }
                    if (awbLabelModel.getConsolidation() != null && Objects.equals(unloc.getLocationsReferenceGUID(), awbLabelModel.getConsolidation().getCarrierDetails().getDestinationPort())) {
                        dictionary.put(ReportConstants.DESTINATION, unloc.getLocCode());
                    }
                }
            }
        }
        dictionary.put(ReportConstants.AIR_LABEL_REMARKS, awbLabelModel.getRemarks());
        dictionary.put(ReportConstants.TENANT_NAME, awbLabelModel.getTenant() != null ? awbLabelModel.getTenant().getTenantName() : null);
        if(awbLabelModel.shipment != null) {
            String shippingLine = awbLabelModel.getShipment().getCarrierDetails() != null ? awbLabelModel.getShipment().getCarrierDetails().getShippingLine() : "";
            if(!CommonUtils.IsStringNullOrEmpty(shippingLine)) {
                dictionary.put(CARRIER, shippingLine);
            }
            var unit = awbLabelModel.getShipment().getPackingList() == null || awbLabelModel.getShipment().getPackingList().isEmpty() ? "" : awbLabelModel.getShipment().getPackingList().get(0).getChargeableUnit();
            var value = addCommasWithPrecision(getChargeable(awbLabelModel.getShipment().getPackingList()) , 2, false);
            dictionary.put(ReportConstants.TOTAL_WEIGHT_AND_UNIT, value + " " + awbLabelModel.getShipment().getChargeableUnit());
            //TODO
            try {
                List<Packing> packingList = commonUtils.convertToList(awbLabelModel.getShipment().getPackingList(), Packing.class);
                PackSummaryResponse response = packingService.calculatePackSummary(packingList, awbLabelModel.getShipment().getTransportMode(), awbLabelModel.getShipment().getShipmentType(), new ShipmentMeasurementDetailsDto());
                if (response != null)
                    dictionary.put(CHARGEABLE_WEIGHT_UNIT, response.getPacksChargeableWeight());
            }catch (Exception e){
                dictionary.put(CHARGEABLE_WEIGHT_UNIT, "0");
            }
            dictionary.put(PACKS_UNIT, Constants.MPK.equals(awbLabelModel.shipment.getPacksUnit()) ? Constants.PACKAGES : awbLabelModel.shipment.getPacksUnit());
        }
        if(awbLabelModel.getAwb() != null && awbLabelModel.getAwb().getAwbCargoInfo() != null) {
            dictionary.put(ReportConstants.OTHER_INFORMATION, awbLabelModel.getAwb().getAwbCargoInfo().getOtherInfo());
            dictionary.put(ReportConstants.HANDLING_INFO, awbLabelModel.getAwb().getAwbCargoInfo().getHandlingInfo());
        }

        List<UnlocationsResponse> airLegs;
        List<RoutingsModel> routingsModels = null;
        if(awbLabelModel.getConsolidation() != null){
            routingsModels = awbLabelModel.getConsolidation().getRoutingsList() == null ? Collections.emptyList() : awbLabelModel.getConsolidation().getRoutingsList().stream().filter(c -> c.getMode().equals(AIR)).filter(c -> !c.getPod().equalsIgnoreCase(awbLabelModel.getConsolidation().getCarrierDetails().getDestinationPort())).toList();
        }else if(awbLabelModel.getShipment() != null){
            routingsModels = awbLabelModel.shipment.getRoutingsList() == null ? Collections.emptyList() : awbLabelModel.shipment.getRoutingsList().stream().filter(c -> c.getMode().equals(AIR)).filter(c -> !c.getPod().equalsIgnoreCase(awbLabelModel.getShipment().getCarrierDetails().getDestinationPort())).toList();
        }

        if(!CollectionUtils.isEmpty(routingsModels)) {
            airLegs = getUnlocationsResponses(routingsModels.stream().map(RoutingsModel::getPod).distinct().toList());
            Map<String, UnlocationsResponse> locationMap = new HashMap<>();
            if(!airLegs.isEmpty()) {
                for(UnlocationsResponse unlocationsResponse : airLegs) {
                    locationMap.put(unlocationsResponse.getLocationsReferenceGUID(), unlocationsResponse);
                }
            }
            List<String> iataCodeList = new ArrayList<>();
            for (RoutingsModel routingsModel : routingsModels) {
                if(locationMap.containsKey(routingsModel.getPod()) && !iataCodeList.contains(locationMap.get(routingsModel.getPod()).getIataCode())) {
                    iataCodeList.add(locationMap.get(routingsModel.getPod()).getIataCode());
                }
            }
            if(awbLabelModel.getConsolidation() != null) {
                if (!iataCodeList.isEmpty())
                    dictionary.put(ReportConstants.CONSOL_FIRST_LEG_DESTINATION, iataCodeList.get(0));
                if (iataCodeList.size() >= 2)
                    dictionary.put(ReportConstants.CONSOL_SECOND_LEG_DESTIATION, iataCodeList.get(1));
                if (iataCodeList.size() >= 3)
                    dictionary.put(ReportConstants.CONSOL_THIRD_LEG_DESTINATION, iataCodeList.get(2));
            } else {
                if (!iataCodeList.isEmpty())
                    dictionary.put(ReportConstants.FIRST_LEG_DESTINATION, iataCodeList.get(0));
                if (iataCodeList.size() >= 2)
                    dictionary.put(ReportConstants.SECOND_LEG_DESTINATION, iataCodeList.get(1));
                if (iataCodeList.size() >= 3)
                    dictionary.put(ReportConstants.THIRD_LEG_DESTINATION, iataCodeList.get(2));
            }
        }

        if(awbLabelModel.getShipment() != null) {
            StringBuilder sb = new StringBuilder();
            if(awbLabelModel.getShipment().getPackingList() != null) {
                awbLabelModel.getShipment().getPackingList().forEach(packingModel -> {
                    var packs = Integer.parseInt(packingModel.getPacks() == null || packingModel.getPacks().isEmpty() ? "0" : packingModel.getPacks());
                    sb.append(packs).append(" ").append(packingModel.getPacksType()).append('\n');
                });
            }
            dictionary.put(HAWB_NOS_PACKS, sb.toString());

            List<String> unlocoRequests = this.createUnLocoRequestFromShipmentModel(awbLabelModel.getShipment());
            Map<String, UnlocationsResponse> unlocationsMap = masterDataUtils.getLocationData(new HashSet<>(unlocoRequests));

            UnlocationsResponse pol = awbLabelModel.getShipment().getCarrierDetails() != null ? unlocationsMap.get(awbLabelModel.getShipment().getCarrierDetails().getOriginPort()) : null;
            UnlocationsResponse pod = awbLabelModel.getShipment().getCarrierDetails() != null ? unlocationsMap.get(awbLabelModel.getShipment().getCarrierDetails().getDestinationPort()) : null;
            if (pod != null) {
                if(pod.getIataCode() != null)
                    dictionary.put(ReportConstants.POD_AIRPORT_CODE_IN_CAPS, pod.getIataCode().toUpperCase());
                if(pod.getAirPortName() != null)
                    dictionary.put(ReportConstants.DESTINATION_PORT, pod.getAirPortName().toUpperCase());
            }
            if (pol != null) {
                if(pol.getIataCode() != null)
                    dictionary.put(ReportConstants.POL_AIRPORT_CODE_IN_CAPS, pol.getIataCode().toUpperCase());
                if(pol.getAirPortName() != null)
                    dictionary.put(ReportConstants.ORIGIN_PORT, pol.getAirPortName().toUpperCase());
            }
        }

        if(awbLabelModel.getConsolidation() != null){
            var mawb = awbLabelModel.getConsolidation().getMawb();
            populateMawb(dictionary, mawb);
            StringBuilder sb = new StringBuilder();
            if(awbLabelModel.getConsolidation().getShipmentsList() != null) {
                awbLabelModel.getConsolidation().getShipmentsList().forEach(shipment -> {
                    int packs = 0;
                    if(shipment.getPackingList() != null)
                        packs = shipment.getPackingList().stream().mapToInt(c -> Integer.parseInt((c.getPacks() == null || c.getPacks().isEmpty()) ? "0" : c.getPacks())).sum();
                    sb.append(shipment.getHouseBill()).append(" ").append(packs).append('\n');
                });
            }
            dictionary.put(HAWB_NOS_PACKS, sb.toString());
            List<String> unlocoRequests = createUnLocoRequestFromConsolidation(awbLabelModel.getConsolidation());
            Map<String, UnlocationsResponse> unlocationsMap = masterDataUtils.getLocationData(new HashSet<>(unlocoRequests));

            UnlocationsResponse pol = awbLabelModel.getConsolidation().getCarrierDetails() != null ? unlocationsMap.get(awbLabelModel.getConsolidation().getCarrierDetails().getOriginPort()) : null;
            UnlocationsResponse pod = awbLabelModel.getConsolidation().getCarrierDetails() != null ? unlocationsMap.get(awbLabelModel.getConsolidation().getCarrierDetails().getDestinationPort()) : null;

            if (pod != null && pod.getIataCode() != null) {
                dictionary.put(ReportConstants.CONSOL_DESTINATION_AIRPORT_CODE, pod.getIataCode());
                dictionary.put(CONSOL_DESTINATION_AIRPORT_CODE_CAPS, pod.getIataCode().toUpperCase());
            }
            if (pod != null && pod.getAirPortName() != null) {
                dictionary.put(ReportConstants.DESTINATION_PORT_NAME_INCAPS_AIR, pod.getAirPortName().toUpperCase());
            }
            if (pol != null && pol.getIataCode() != null) {
                dictionary.put(ReportConstants.CONSOL_ORIGIN_AIRPORT_CODE, pol.getIataCode());
                dictionary.put(CONSOL_ORIGIN_AIRPORT_CODE_CAPS, pol.getIataCode().toUpperCase());
            }
            if (pol != null && pol.getAirPortName() != null) {
                dictionary.put(ReportConstants.ORIGIN_PORT_NAME_INCAPS_AIR, pol.getAirPortName().toUpperCase());
            }
        }

        if(awbLabelModel.getAwb() != null) {
            if(awbLabelModel.getConsolidation() == null)
                dictionary.put(ReportConstants.HAWB_CAPS, awbLabelModel.getShipment().getMasterBill());
            else {
                dictionary.put(HAWB_CAPS, awbLabelModel.getConsolidation().getShipmentsList().stream().map(ShipmentModel::getHouseBill).collect(Collectors.joining(", ")));
                dictionary.put(ReportConstants.MAWB_CAPS, awbLabelModel.getConsolidation().getMawb());
            }
        }

        if(awbLabelModel.getConsolidation() != null && awbLabelModel.getConsolidation().getPackingList() != null) {
            try {
                List<Packing> packingList = commonUtils.convertToList(awbLabelModel.getConsolidation().getPackingList(), Packing.class);
                PackSummaryResponse response = packingService.calculatePackSummary(packingList, awbLabelModel.getConsolidation().getTransportMode(), awbLabelModel.getConsolidation().getContainerCategory(), new ShipmentMeasurementDetailsDto());
                if(response != null)
                    dictionary.put(ReportConstants.CONSOL_CHARGEABLE_WEIGHT_AND_UNIT, response.getPacksChargeableWeight());
            }catch (Exception e){
                dictionary.put(CONSOL_CHARGEABLE_WEIGHT_AND_UNIT, "0");
            }
        }

        if (awbLabelModel.getConsolidation() != null && awbLabelModel.getConsolidation().getPackingList() != null) {
            var totalPacks = 0;
            int size = awbLabelModel.getConsolidation().getPackingList().size();
            for(int i = 0; i < size; i++) {
                if(awbLabelModel.getConsolidation().getPackingList().get(i).getPacks() != null)
                    totalPacks += Integer.parseInt(awbLabelModel.getConsolidation().getPackingList().get(i).getPacks());
            }
            dictionary.put(ReportConstants.TOTAL_CONSOL_PACKS, totalPacks);
            var packingModelList = awbLabelModel.getConsolidation().getPackingList().stream().filter(c -> c.getWeight() != null && c.getWeightUnit() != null).toList();
            var value = addCommasWithPrecision(BigDecimal.valueOf(packingModelList.stream().mapToDouble(c -> c.getWeight().doubleValue()).sum()) , 2, false);
            dictionary.put(CONSOL_TOTAL_WEIGHT_AND_UNIT,  value + " " + (!packingModelList.isEmpty() ? packingModelList.get(0).getWeightUnit() : null));
        }
        if (awbLabelModel.shipment != null && awbLabelModel.shipment.getPackingList() != null) {
            dictionary.put(ReportConstants.TOTAL_PACKS, awbLabelModel.shipment.getNoOfPacks());
        }
        if(isCombi) {
            dictionary.put(IS_COMBI, true);
            dictionary.put(ReportConstants.IS_MAWB, false);
            dictionary.put(ReportConstants.IS_HAWB, false);
            List<org.apache.commons.lang3.tuple.Pair<String, Integer>> hawbPacksList = new ArrayList<>();
            if(!listIsNullOrEmpty(awbLabelModel.getShipmentModels())) {
                for(ShipmentModel shipmentModel: awbLabelModel.getShipmentModels()) {
                    int noOfPacks = 0;
                    if(!listIsNullOrEmpty(shipmentModel.getPackingList())) {
                        for(PackingModel packingModel: shipmentModel.getPackingList()) {
                            noOfPacks = noOfPacks + Integer.parseInt(packingModel.getPacks());
                        }
                    }
                    if(noOfPacks > 0)
                        hawbPacksList.add(Pair.of(shipmentModel.getHouseBill(), noOfPacks));
                }
            }
            dictionary.put("hawbPacksMap", hawbPacksList);
        } else {
            dictionary.put(IS_COMBI, false);
            dictionary.put(ReportConstants.IS_MAWB, this.isMawb);
            dictionary.put(ReportConstants.IS_HAWB, !this.isMawb);
        }
        if(awbLabelModel.getShipment() != null && isMawb){
            //DRT SHIPMENT
            dictionary.put(AIRLINE_NAME, dictionary.get(CARRIER));
            dictionary.put(CONSOL_DESTINATION_AIRPORT_CODE, dictionary.get(POD_AIRPORT_CODE_IN_CAPS));
            dictionary.put(DESTINATION_PORT_NAME_INCAPS_AIR, dictionary.get(DESTINATION_PORT));
            dictionary.put(CONSOL_ORIGIN_AIRPORT_CODE, dictionary.get(POL_AIRPORT_CODE_IN_CAPS));
            dictionary.put(ORIGIN_PORT_NAME_INCAPS_AIR, dictionary.get(ORIGIN_PORT));
            dictionary.put(TOTAL_CONSOL_PACKS, dictionary.get(TOTAL_PACKS));
            dictionary.put(CONSOL_FIRST_LEG_DESTINATION, dictionary.get(FIRST_LEG_DESTINATION));
            dictionary.put(CONSOL_SECOND_LEG_DESTIATION, dictionary.get(SECOND_LEG_DESTINATION));
            dictionary.put(CONSOL_THIRD_LEG_DESTINATION, dictionary.get(THIRD_LEG_DESTINATION));
            dictionary.put(CONSOL_CHARGEABLE_WEIGHT_AND_UNIT, dictionary.get(CHARGEABLE_WEIGHT_UNIT));
            dictionary.put(MAWB_CAPS, awbLabelModel.getShipment().getMasterBill());
            dictionary.remove(HAWB_CAPS);
        }
        return dictionary;
    }

    public String getConsolGrossWeightAndUnit(ConsolidationModel consolidationModel) throws RunnerException {
        List<Packing> packingList = commonUtils.convertToList(consolidationModel.getPackingList(), Packing.class);
        PackSummaryResponse response = packingService.calculatePackSummary(packingList, consolidationModel.getTransportMode(), consolidationModel.getContainerCategory(), new ShipmentMeasurementDetailsDto());
        return response.getTotalPacksWeight();
    }

    public static void populateMawb(Map<String, Object> dictionary, String mawb) {
        if (mawb != null) {
            mawb = mawb.replace("-", "");
            if (mawb.length() < 11) mawb = appendZero(mawb, 11);
            if (mawb.length() >= 3) dictionary.put(ReportConstants.MAWB13, mawb.substring(0, 3));
            if (mawb.length() >= 7) dictionary.put(ReportConstants.MAWB47, mawb.substring(3, 7));
            if (mawb.length() >= 11) dictionary.put(ReportConstants.MAWB811, mawb.substring(7));
            dictionary.put(ReportConstants.MAWB_NUMBER, mawb);
        } else {
            dictionary.put(ReportConstants.MAWB_NUMBER, null);
        }
    }

    private BigDecimal getChargeable(List<PackingModel> packingList) {
        if(packingList == null || packingList.isEmpty())
            return BigDecimal.ZERO;
        double value = 0;
        for(var pack : packingList){
            if(pack.getChargeable() != null)
                value += pack.getChargeable().doubleValue();
        }
        return BigDecimal.valueOf(value);
    }

    private List<UnlocationsResponse> getUnlocationsResponses(List<String> unlocations) {
        if(unlocations.isEmpty()) return Collections.emptyList();
        List<Object> criteria = Arrays.asList(
                Arrays.asList(EntityTransferConstants.LOCATION_SERVICE_GUID),
                "In",
                Arrays.asList(unlocations)
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(criteria).build();
        V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
        return jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
    }
}
