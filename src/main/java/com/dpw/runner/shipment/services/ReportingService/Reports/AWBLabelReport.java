package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.AWbLabelModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.RoutingsModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;

@Component
public class AWBLabelReport extends IReport{
    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IV1Service v1Service;

    public void setMawb(boolean mawb) {
        isMawb = mawb;
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

    private boolean isMawb;
    private String remarks;
    @Override
    public Map<String, Object> getData(Long id) {
        AWbLabelModel awbLabelModel = (AWbLabelModel) getDocumentModel(id);
        return populateDictionary(awbLabelModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) {
        AWbLabelModel awbLabelModel = new AWbLabelModel();
        if(isMawb) {
            awbLabelModel.setConsolidation(getConsolidation(id));
            awbLabelModel.setAwb(getMawb(id));
        }
        else {
            awbLabelModel.shipment = getShipment(id);
            awbLabelModel.setAwb(getHawb(id));
            validateAirDGCheck(awbLabelModel.shipment);
        }
        awbLabelModel.tenant = getTenant();
        awbLabelModel.setRemarks(this.remarks);
        return awbLabelModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        AWbLabelModel awbLabelModel = (AWbLabelModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();
        List<String> unlocations = new ArrayList<>();
        if(awbLabelModel.shipment != null) {
            String mawb = awbLabelModel.shipment.getMasterBill();
            V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
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
        }
        if(awbLabelModel.getConsolidation() != null){
            dictionary.put(AIRLINE_NAME, awbLabelModel.getConsolidation().getCarrierDetails() != null ? awbLabelModel.getConsolidation().getCarrierDetails().getShippingLine() : null);
            if(awbLabelModel.getConsolidation().getCarrierDetails() != null && awbLabelModel.getConsolidation().getCarrierDetails().getDestination() != null)
                unlocations.add(awbLabelModel.getConsolidation().getCarrierDetails().getDestination());

            if(awbLabelModel.getConsolidation().getCarrierDetails() != null && awbLabelModel.getConsolidation().getCarrierDetails().getDestinationPort() != null)
                unlocations.add(awbLabelModel.getConsolidation().getCarrierDetails().getDestinationPort());
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
            dictionary.put(CARRIER, awbLabelModel.getShipment().getCarrierDetails() != null ? awbLabelModel.getShipment().getCarrierDetails().getShippingLine() : null);
            dictionary.put(ReportConstants.TOTAL_WEIGHT_AND_UNIT, awbLabelModel.getShipment().getChargable() + " " + awbLabelModel.getShipment().getChargeableUnit());
            dictionary.put(PACKS_UNIT, Constants.MPK.equals(awbLabelModel.shipment.getPacksUnit()) ? Constants.PACKAGES : awbLabelModel.shipment.getPacksUnit());
        }
        if(awbLabelModel.getAwb() != null && awbLabelModel.getAwb().getAwbCargoInfo() != null) {
            dictionary.put(ReportConstants.OTHER_INFORMATION, awbLabelModel.getAwb().getAwbCargoInfo().getOtherInfo());
            dictionary.put(ReportConstants.HANDLING_INFO, awbLabelModel.getAwb().getAwbCargoInfo().getHandlingInfo());
        }

        List<UnlocationsResponse> airLegs;
        if(awbLabelModel.getConsolidation() != null){
            unlocations = awbLabelModel.getConsolidation().getRoutingsList() == null ? Collections.emptyList() : awbLabelModel.getConsolidation().getRoutingsList().stream().filter(c -> c.getMode().equals(AIR)).map(RoutingsModel::getPod).distinct().toList();
            airLegs = getUnlocationsResponses(unlocations);
            if(!airLegs.isEmpty()) dictionary.put(ReportConstants.CONSOL_FIRST_LEG_DESTINATION, airLegs.get(0).getIataCode());
            if(airLegs.size() >= 2) dictionary.put(ReportConstants.CONSOL_SECOND_LEG_DESTIATION, airLegs.get(1).getIataCode());
            if(airLegs.size() >= 3) dictionary.put(ReportConstants.CONSOL_THIRD_LEG_DESTINATION, airLegs.get(2).getIataCode());
        }
        if(awbLabelModel.getShipment() != null){
            unlocations = awbLabelModel.shipment.getRoutingsList() == null ? Collections.emptyList() : awbLabelModel.shipment.getRoutingsList().stream().filter(c -> c.getMode().equals(AIR)).map(RoutingsModel::getPod).distinct().toList();
            airLegs = getUnlocationsResponses(unlocations);
            if(!airLegs.isEmpty()) dictionary.put(ReportConstants.FIRST_LEG_DESTINATION, airLegs.get(0).getIataCode());
            if(airLegs.size() >= 2) dictionary.put(ReportConstants.SECOND_LEG_DESTINATION, airLegs.get(1).getIataCode());
            if(airLegs.size() >= 3) dictionary.put(ReportConstants.THIRD_LEG_DESTINATION, airLegs.get(2).getIataCode());
        }

        if(awbLabelModel.getShipment() != null) {
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
            StringBuilder sb = new StringBuilder();
            awbLabelModel.getConsolidation().getShipmentsList().forEach(shipment -> sb.append(shipment.getHouseBill()).append(" ").append(shipment.getNoOfPacks()).append('\n'));
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
                dictionary.put(ReportConstants.HAWB_CAPS, awbLabelModel.getAwb().getAwbNumber());
            else
                dictionary.put(HAWB_CAPS, awbLabelModel.getConsolidation().getShipmentsList().stream().map(ShipmentModel::getHouseBill).collect(Collectors.joining(", ")));
            dictionary.put(ReportConstants.MAWB_CAPS, awbLabelModel.getAwb().getAwbNumber());
        }

        if(awbLabelModel.getConsolidation() != null && awbLabelModel.getConsolidation().getAllocations() != null)
            dictionary.put(ReportConstants.CONSOL_CHARGEABLE_WEIGHT_AND_UNIT, awbLabelModel.getConsolidation().getAllocations().getChargable() + " " + awbLabelModel.getConsolidation().getAllocations().getChargeableUnit());

        if (awbLabelModel.getConsolidation() != null && awbLabelModel.getConsolidation().getPackingList() != null) {
            var totalPacks = 0;
            int size = awbLabelModel.getConsolidation().getPackingList().size();
            for(int i = 0; i < size; i++) {
                if(awbLabelModel.getConsolidation().getPackingList().get(i).getPacks() != null)
                    totalPacks += Integer.parseInt(awbLabelModel.getConsolidation().getPackingList().get(i).getPacks());
            }
            dictionary.put(ReportConstants.TOTAL_CONSOL_PACKS, totalPacks);
            var packingModelList = awbLabelModel.getConsolidation().getPackingList().stream().filter(c -> c.getWeight() != null && c.getWeightUnit() != null).toList();
            dictionary.put(CONSOL_TOTAL_WEIGHT_AND_UNIT, packingModelList.stream().mapToDouble(c -> c.getWeight().doubleValue()).sum() + " " + (!packingModelList.isEmpty() ? packingModelList.get(0).getWeightUnit() : null));
        }
        if (awbLabelModel.shipment != null && awbLabelModel.shipment.getPackingList() != null) {
            var totalPacks = 0;
            int size = awbLabelModel.shipment.getPackingList().size();
            for(int i = 0; i < size; i++) {
                if(awbLabelModel.shipment.getPackingList().get(i).getPacks() != null)
                    totalPacks += Integer.parseInt(awbLabelModel.shipment.getPackingList().get(i).getPacks());
            }
            dictionary.put(ReportConstants.TOTAL_PACKS, totalPacks);
        }
        dictionary.put(ReportConstants.IS_MAWB, this.isMawb);
        dictionary.put(ReportConstants.IS_HAWB, !this.isMawb);
        return dictionary;
    }

    private List<UnlocationsResponse> getUnlocationsResponses(List<String> unlocations) {
        List<Object> criteria = Arrays.asList(
                Arrays.asList(EntityTransferConstants.LOCATION_SERVICE_GUID),
                "In",
                Arrays.asList(unlocations)
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
        return jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
    }
}
