package com.dpw.runner.shipment.services.ReportingService.Reports;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FCR_DATE_OF_ISSUE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FCR_NO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FCR_PLACE_OF_ISSUE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PLACE_OF_ISSUE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_DETAIL_DATE_OF_ISSUE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_NO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPPED_ONBOARD_DATE_DDMMMYYYY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPPED_ONBOARD_TEXT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIP_CONSIGNEE_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIP_NOTIFY_PARTY_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.USA_LOC_CODE_PREFIX;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getOrgAddressDetails;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.FCRDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.lang3.ObjectUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class FCRDocumentReport extends IReport{

    @Autowired
    private ModelMapper modelMapper;

    public void setFcrShipper(PartiesRequest fcrShipper1) {
        this.fcrShipper = fcrShipper1;
    }

    public void setPackIds(List<Long> packIds1) {
        this.packIds = packIds1;
    }
    public void setPlaceOfIssue(String placeOfIssue) {
        this.placeOfIssue = placeOfIssue;
    }
    public void setIssueDate(LocalDateTime issueDate) {
        this.issueDate = issueDate;
    }

    private PartiesRequest fcrShipper;

    private List<Long> packIds;
    private String placeOfIssue;
    private LocalDateTime issueDate;
    @Override
    public Map<String, Object> getData(Long id) throws RunnerException {
        FCRDocumentModel fcrDocumentModel = (FCRDocumentModel) getDocumentModel(id);
        return populateDictionary(fcrDocumentModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) throws RunnerException {
        FCRDocumentModel fcrDocumentModel = new FCRDocumentModel();
        fcrDocumentModel.setShipmentModel(getShipment(id));
        return fcrDocumentModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) throws RunnerException {
        FCRDocumentModel fcrDocumentModel = (FCRDocumentModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();
        Set<String> unlocationCodeSet = new HashSet<>();
        if(fcrShipper != null)
            dictionary.put(ReportConstants.FCR_CONSIGNOR_IN_CAPS, getOrgAddressDetails(modelMapper.map(fcrShipper, PartiesModel.class)));
        dictionary.put(SHIP_CONSIGNEE_IN_CAPS, getOrgAddressDetails(fcrDocumentModel.getShipmentModel().getConsignee()));
        dictionary.put(SHIP_NOTIFY_PARTY_IN_CAPS, getOrgAddressDetails(fcrDocumentModel.getShipmentModel().getAdditionalDetails().getNotifyParty()));
        dictionary.put(SHIPMENT_NO, fcrDocumentModel.getShipmentModel().getShipmentId());
        dictionary.put(FCR_NO, "FCR " +
                fcrDocumentModel.getShipmentModel().getShipmentId() +
                String.format("%03d",
                        (fcrDocumentModel.getShipmentModel().getAdditionalDetails().getFcrNumber() != null ? fcrDocumentModel.getShipmentModel().getAdditionalDetails().getFcrNumber() : 0) + 1));
        fcrDocumentModel.getShipmentModel().setPackingList(
                fcrDocumentModel.getShipmentModel()
                        .getPackingList()
                        .stream()
                        .filter(e -> packIds.contains(e.getId()))
                        .collect(Collectors.toList())
        );
        getPackingDetails(fcrDocumentModel.getShipmentModel(), dictionary);
        unlocationCodeSet.add(fcrDocumentModel.getShipmentModel().getAdditionalDetails().getPlaceOfIssue());
        unlocationCodeSet.add(this.placeOfIssue);
        Map<String, EntityTransferUnLocations> unLocationsMap = masterDataUtils.getLocationDataFromCache(unlocationCodeSet, EntityTransferConstants.LOCATION_SERVICE_GUID);
        if(!CommonUtils.isStringNullOrEmpty(fcrDocumentModel.getShipmentModel().getAdditionalDetails().getPlaceOfIssue())) {
            dictionary.put(PLACE_OF_ISSUE, unLocationsMap.get(fcrDocumentModel.getShipmentModel().getAdditionalDetails().getPlaceOfIssue()).Name);
        }
        populateFcrPlaceOfIssue(dictionary, unLocationsMap);
        dictionary.put(SHIPMENT_DETAIL_DATE_OF_ISSUE, convertToDPWDateFormat(fcrDocumentModel.getShipmentModel().getAdditionalDetails().getDateOfIssue()));
        dictionary.put(FCR_DATE_OF_ISSUE, convertToDPWDateFormat(this.issueDate));

        dictionary.put(SHIPPED_ONBOARD_TEXT, fcrDocumentModel.getShipmentModel().getAdditionalDetails().getShippedOnboardText().toUpperCase());
        dictionary.put(SHIPPED_ONBOARD_DATE_DDMMMYYYY, convertToDPWDateFormat(
                fcrDocumentModel.getShipmentModel().getAdditionalDetails().getShippedOnboardDate(), "ddMMMyyyy".toUpperCase(), false));


        if (fcrDocumentModel.getShipmentModel() != null && ObjectUtils.isNotEmpty(fcrDocumentModel.getShipmentModel().getConsolidationList())) {
            ConsolidationModel consolidationModel = fcrDocumentModel.getShipmentModel().getConsolidationList().get(0);
            this.populateConsolidationReportData(dictionary, null, consolidationModel.getId());
        }

        this.populateShipmentReportData(dictionary, null, fcrDocumentModel.getShipmentModel().getId());
        this.getContainerDetails(fcrDocumentModel.getShipmentModel(), dictionary);
        this.getPackingDetails(fcrDocumentModel.getShipmentModel(), dictionary);
        return convertValuesToUpperCase(dictionary);
    }

    private void populateFcrPlaceOfIssue(Map<String, Object> dictionary, Map<String, EntityTransferUnLocations> unLocationsMap) {
        if(StringUtility.isEmpty(this.placeOfIssue)) return;

        EntityTransferUnLocations unLocations = Optional.ofNullable(unLocationsMap.get(this.placeOfIssue)).orElse(new EntityTransferUnLocations());
        StringBuilder sb = new StringBuilder(Optional.ofNullable(unLocations.getCityName()).orElse(Constants.EMPTY_STRING));
        // USA region -> display city name with state code
        if (StringUtility.convertToString(unLocations.getLocCode()).startsWith(USA_LOC_CODE_PREFIX) && !StringUtility.isEmpty(unLocations.getState())) {
            sb.append(", ").append(unLocations.getState());
        }

        dictionary.put(FCR_PLACE_OF_ISSUE, sb.toString());
    }


    public static Map<String, Object> convertValuesToUpperCase(Map<String, Object> map) {
        Map<String, Object> result = new HashMap<>();
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (value instanceof String) {
                result.put(key, StringUtility.toUpperCase((String) value));
            } else if (value instanceof List) {
                // Convert a list of strings to uppercase
                List<?> list = (List<?>) value;
                result.put(key, convertListToUpperCase(list));
            } else if (value instanceof Map) {
                // Recursively handle nested maps
                Map<?, ?> nestedMap = (Map<?, ?>) value;
                result.put(key, convertValuesToUpperCase((Map<String, Object>) nestedMap));
            } else {
                // Keep other types as is
                result.put(key, value);
            }
        }
        return result;
    }

    private static List<?> convertListToUpperCase(List<?> list) {
        List<Object> result = new ArrayList<>();
        for (Object item : list) {
            if (item instanceof String) {
                result.add(StringUtility.toUpperCase((String) item));
            } else if (item instanceof Map) {
                // Recursively handle nested maps in lists
                result.add(convertValuesToUpperCase((Map<String, Object>) item));
            } else {
                // Keep other types as is
                result.add(item);
            }
        }
        return result;
    }
}
