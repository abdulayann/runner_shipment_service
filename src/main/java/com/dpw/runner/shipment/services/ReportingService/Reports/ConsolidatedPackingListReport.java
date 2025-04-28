package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.ConsolidatedPackingListModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PackingModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.core.type.TypeReference;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.*;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.*;
import static com.dpw.runner.shipment.services.utils.CommonUtils.stringValueOf;

@Component
public class ConsolidatedPackingListReport extends IReport {

    @Autowired
    JsonHelper jsonHelper;


/**
*
   * @param id Consolidation Id
   * @return dictionary
*/
    @Override
    public Map<String, Object> getData(Long id){
        ConsolidatedPackingListModel cplData = (ConsolidatedPackingListModel) getDocumentModel(id);
        return populateDictionary(cplData);
    }

/**
*
   * @param id Consolidation Id
   * @return ConsolidatedPackingList model with populated fields
*/
    @Override
    IDocumentModel getDocumentModel(Long id) {
        ConsolidatedPackingListModel cplData = new ConsolidatedPackingListModel();
        cplData.setConsolidationDetails(getConsolidation(id));
        cplData.setTenant(getTenant());
        return cplData;
    }

/**
*
   * @param documentModel The document model containing the consolidation details.
   * @return dictionary generated from consolidation details
*/
    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        ConsolidatedPackingListModel cplData = (ConsolidatedPackingListModel) documentModel;
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String json = jsonHelper.convertToJsonWithDateTimeFormatter(cplData.getConsolidationDetails(), getDPWDateFormatOrDefault(v1TenantSettingsResponse));
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);

        List<String> exporter = getPartiesDetails(cplData.getConsolidationDetails().getSendingAgent());

        List<String> consignee = getPartiesDetails(cplData.getConsolidationDetails().getReceivingAgent());

        addTenantDetails(dictionary, cplData.getTenant());
        List<String> tenantsDataList = getListOfStrings(cplData.getTenant().tenantName, cplData.getTenant().address1,
                cplData.getTenant().address2, cplData.getTenant().city, cplData.getTenant().state,
                cplData.getTenant().zipPostCode, cplData.getTenant().country, cplData.getTenant().email,
                cplData.getTenant().websiteUrl, cplData.getTenant().phone);
        if(!tenantsDataList.isEmpty())
            dictionary.put(ReportConstants.TENANT, tenantsDataList);
        dictionary.put(EXPORTER, exporter);
        dictionary.put(CONSIGNEE, consignee);

        processAgents(cplData.getConsolidationDetails().getSendingAgent(), dictionary, EXPORT_AGENT_FREETEXT, exporter, EXPORTER_TAX_ID);

        processAgents(cplData.getConsolidationDetails().getReceivingAgent(), dictionary, IMPORT_AGENT_FREETEXT, consignee, CONSIGNEE_TAX_ID);

        var etd = cplData.getConsolidationDetails().getCarrierDetails().getEtd();

        dictionary.put(SHIP_DATE, convertToDPWDateFormat(etd));
        dictionary.put(AIRWAY_BILL_NUMBER, cplData.getConsolidationDetails().getBol());

        boolean flag = false;
        if(cplData.getConsolidationDetails().getReferenceNumbersList() != null){
            for (var i : cplData.getConsolidationDetails().getReferenceNumbersList()) {
                if(i.getType().equals(INVNO)){
                    dictionary.put(INVOICE_NUMBER, i.getReferenceNumber());
                    flag = true;
                }
            }
        }

        if(!flag)
            dictionary.put(INVOICE_NUMBER, null);
        dictionary.put(PURCHASE_ORDER_NUMBER, cplData.getConsolidationDetails().getReferenceNumber());
        dictionary.put(PAYMENT_TERMS, cplData.getConsolidationDetails().getPayment());

    List<PackingModel> packingList = getPackingModelDetails(cplData);
    processPackingListAndCalculateTotals(dictionary, packingList, v1TenantSettingsResponse);

    dictionary.put(PACKAGE_TYPE, cplData.getConsolidationDetails().getPackageType());
    dictionary.put(SPECIAL_INSTRUCTION, cplData.getConsolidationDetails().getSpecialInstructions());
    dictionary.put(TRANSPORT_MODE, cplData.getConsolidationDetails().getTransportMode());

    return dictionary;
    }

    private void processPackingListAndCalculateTotals(Map<String, Object> dictionary, List<PackingModel> packingList, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if (packingList == null || packingList.isEmpty()) {
            setEmptyTotals(dictionary);
            return;
        }

        List<Map<String, Object>> values = jsonHelper.convertValue(new HashSet<>(packingList), new TypeReference<>() {});
        processItemWeights(values, v1TenantSettingsResponse);
        calculateAndSetTotals(dictionary, values, v1TenantSettingsResponse);
    }

    private void processItemWeights(List<Map<String, Object>> values, V1TenantSettingsResponse v1TenantSettingsResponse) {
        values.forEach(v -> {
            if (v.get(WEIGHT) != null) {
                v.put(WEIGHT, convertToWeightNumberFormat(v.get(WEIGHT), v1TenantSettingsResponse));
            }
        });
    }

    private void calculateAndSetTotals(Map<String, Object> dictionary, List<Map<String, Object>> values, V1TenantSettingsResponse v1TenantSettingsResponse) {
        long totalPacks = 0L;
        BigDecimal totalWeight = BigDecimal.ZERO;
        String unitOfTotalWeight = null;
        boolean weightUnitConsistent = true;

        for (var v : values) {
            if (v.get(PACKS) != null) {
                totalPacks += Long.parseLong(v.get(PACKS).toString());
            }

            if (v.get(WEIGHT) != null && v.get(WEIGHT_UNIT) != null) {
                String currentUnit = v.get(WEIGHT_UNIT).toString();
                BigDecimal currentWeight = new BigDecimal(stringValueOf(v.get(WEIGHT)));

                if (unitOfTotalWeight == null) {
                    unitOfTotalWeight = currentUnit;
                    totalWeight = totalWeight.add(currentWeight);
                } else if (unitOfTotalWeight.equals(currentUnit)) {
                    totalWeight = totalWeight.add(currentWeight);
                } else {
                    weightUnitConsistent = false;
                }
            }
        }

        dictionary.put(ITEMS, values);
        dictionary.put(TOTAL_PACKS, totalPacks != 0 ? totalPacks : null);

        if (!weightUnitConsistent || totalWeight.equals(BigDecimal.ZERO)) {
            setEmptyWeight(dictionary);
        } else {
            dictionary.put(TOTAL_PACKS_WEIGHT, convertToWeightNumberFormat(totalWeight, v1TenantSettingsResponse));
            dictionary.put(UOTW, unitOfTotalWeight);
        }
    }

    private void setEmptyTotals(Map<String, Object> dictionary) {
        dictionary.put(ITEMS, Collections.emptyList());
        dictionary.put(TOTAL_PACKS, null);
        dictionary.put(TOTAL_PACKS_WEIGHT, null);
        dictionary.put(UOTW, null);
    }

    private void setEmptyWeight(Map<String, Object> dictionary){
        dictionary.put(TOTAL_PACKS_WEIGHT, null);
        dictionary.put(UOTW, null);
    }

    private void processAgents(PartiesModel cplData, Map<String, Object> dictionary, String exportAgentFreetext, List<String> exporter, String exporterTaxId) {
        if (cplData != null && cplData.getAddressData() != null && cplData.getAddressData().containsKey(PartiesConstants.RAW_DATA)) {
            dictionary.put(exportAgentFreetext, getAddressList(StringUtility.convertToString(cplData.getAddressData().get(PartiesConstants.RAW_DATA))));
        } else {
            dictionary.put(exportAgentFreetext, exporter);
        }
        if (cplData != null && cplData.getOrgData() != null)
            dictionary.put(exporterTaxId, cplData.getOrgData().get(TENANT_VATREGNUMBER));
    }

    private List<PackingModel> getPackingModelDetails(ConsolidatedPackingListModel cplData) {
        List<PackingModel> packingList = new ArrayList<>();
        if(cplData.getConsolidationDetails().getPackingList() != null)
            packingList.addAll(cplData.getConsolidationDetails().getPackingList());
        List<ShipmentModel> shipments = cplData.getConsolidationDetails().getShipmentsList();

        for (var shipment : shipments){
            if(shipment.getPackingList() != null)
                packingList.addAll(shipment.getPackingList());
        }
        return packingList;
    }

    private List<String> getPartiesDetails(PartiesModel cplData) {
        List<String> exporter = getOrgAddressWithPhoneEmail(jsonHelper.convertValue(
                cplData, PartiesModel.class
        ));
        if (cplData != null) {
            Map<String, Object> orgData = cplData.getOrgData();
            if (getValueFromMap(orgData, ReportConstants.FULL_NAME) != null)
                exporter.add(0, getValueFromMap(orgData, ReportConstants.FULL_NAME));
        }
        return exporter;
    }
}
