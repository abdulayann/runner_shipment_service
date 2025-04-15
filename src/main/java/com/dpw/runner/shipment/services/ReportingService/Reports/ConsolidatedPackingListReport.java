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
import static java.lang.Long.sum;

@Component
public class ConsolidatedPackingListReport extends IReport {

    @Autowired
    JsonHelper jsonHelper;


/**
*
   * @param id
   * @return dictionary
*/
    @Override
    public Map<String, Object> getData(Long id){
        ConsolidatedPackingListModel cplData = (ConsolidatedPackingListModel) getDocumentModel(id);
        return populateDictionary(cplData);
    }

/**
*
   * @param id
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
   * @param documentModel
   * @return dictionary generated from consolidation details
*/
    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        ConsolidatedPackingListModel cplData = (ConsolidatedPackingListModel) documentModel;
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String json = jsonHelper.convertToJsonWithDateTimeFormatter(cplData.getConsolidationDetails(), GetDPWDateFormatOrDefault(v1TenantSettingsResponse));
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);

        List<String> exporter = getOrgAddressWithPhoneEmail(jsonHelper.convertValue(
                cplData.getConsolidationDetails().getSendingAgent(), PartiesModel.class
        ));
        if(cplData.getConsolidationDetails().getSendingAgent() != null){
            Map<String, Object> orgData = cplData.getConsolidationDetails().getSendingAgent().getOrgData();
            if(getValueFromMap(orgData, ReportConstants.FULL_NAME) != null)
                exporter.add(0, getValueFromMap(orgData, ReportConstants.FULL_NAME));
        }

        List<String> consignee = getOrgAddressWithPhoneEmail(jsonHelper.convertValue(
                cplData.getConsolidationDetails().getReceivingAgent(), PartiesModel.class
        ));
        if(cplData.getConsolidationDetails().getReceivingAgent() != null){
            Map<String, Object> orgData = cplData.getConsolidationDetails().getReceivingAgent().getOrgData();
            if(getValueFromMap(orgData, ReportConstants.FULL_NAME) != null)
                consignee.add(0, getValueFromMap(orgData, ReportConstants.FULL_NAME));
        }

        addTenantDetails(dictionary, cplData.getTenant());
        List<String> tenantsDataList = getListOfStrings(cplData.getTenant().tenantName, cplData.getTenant().address1,
                cplData.getTenant().address2, cplData.getTenant().city, cplData.getTenant().state,
                cplData.getTenant().zipPostCode, cplData.getTenant().country, cplData.getTenant().email,
                cplData.getTenant().websiteUrl, cplData.getTenant().phone);
        if(!tenantsDataList.isEmpty())
            dictionary.put(ReportConstants.TENANT, tenantsDataList);
        dictionary.put(EXPORTER, exporter);
        dictionary.put(CONSIGNEE, consignee);

        PartiesModel sendingAgent = cplData.getConsolidationDetails().getSendingAgent();
        if (sendingAgent != null && sendingAgent.getAddressData() != null && sendingAgent.getAddressData().containsKey(PartiesConstants.RAW_DATA)) {
            dictionary.put(EXPORT_AGENT_FREETEXT, getAddressList(StringUtility.convertToString(sendingAgent.getAddressData().get(PartiesConstants.RAW_DATA))));
        } else {
            dictionary.put(EXPORT_AGENT_FREETEXT, exporter);
        }
        if (sendingAgent != null && sendingAgent.getOrgData() != null)
            dictionary.put(EXPORTER_TAX_ID, sendingAgent.getOrgData().get(TENANT_VATREGNUMBER));

        PartiesModel receivingAgent = cplData.getConsolidationDetails().getReceivingAgent();
        if (receivingAgent != null && receivingAgent.getAddressData() != null && receivingAgent.getAddressData().containsKey(PartiesConstants.RAW_DATA)) {
            dictionary.put(IMPORT_AGENT_FREETEXT, getAddressList(StringUtility.convertToString(receivingAgent.getAddressData().get(PartiesConstants.RAW_DATA))));
        } else {
            dictionary.put(IMPORT_AGENT_FREETEXT, consignee);
        }

        if (receivingAgent != null && receivingAgent.getOrgData() != null)
            dictionary.put(CONSIGNEE_TAX_ID, receivingAgent.getOrgData().get(TENANT_VATREGNUMBER));

        var etd = cplData.getConsolidationDetails().getCarrierDetails().getEtd();

        dictionary.put(SHIP_DATE, ConvertToDPWDateFormat(etd));
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

        List<PackingModel> packingList = new ArrayList<>();
        if(cplData.getConsolidationDetails().getPackingList() != null)
            packingList.addAll(cplData.getConsolidationDetails().getPackingList());
        long totalPacks = 0L;
        BigDecimal totalWeight = BigDecimal.ZERO;
        String unitOfTotalWeight = null;
        boolean breakFlagForWeight = false;
        List<ShipmentModel> shipments = cplData.getConsolidationDetails().getShipmentsList();

        for (var shipment : shipments){
            if(shipment.getPackingList() != null)
                packingList.addAll(shipment.getPackingList());
        }

        if(packingList.size() > 0) {
//            String packingJson = jsonHelper.convertToJson(packingList);
//            var values = jsonHelper.convertValue(packingJson, new TypeReference<List<Map<String, Object>>>() {});
            Set<PackingModel> packingSet = new HashSet<>(packingList);
            List<Map<String, Object>> values = jsonHelper.convertValue(packingSet, new TypeReference<>() {});
            for (var v : values) {
                totalPacks = sum(totalPacks, Long.parseLong(v.get(PACKS).toString()));
                if (!breakFlagForWeight && v.get(WEIGHT) != null && v.get(WEIGHT_UNIT) != null) {
                    if (unitOfTotalWeight == null) {
                        unitOfTotalWeight = v.get(WEIGHT_UNIT).toString();
                        totalWeight = totalWeight.add(v.get(WEIGHT) != null ? new BigDecimal(stringValueOf(v.get(WEIGHT))) : BigDecimal.ZERO);
                    } else if (!unitOfTotalWeight.equals(v.get(WEIGHT_UNIT).toString())) {
                        totalWeight = BigDecimal.ZERO;
                        breakFlagForWeight = true;
                    }
                    else
                        totalWeight = totalWeight.add(v.get(WEIGHT) != null ? new BigDecimal(stringValueOf(v.get(WEIGHT))) : BigDecimal.ZERO);
                }
                if(v.get(WEIGHT) != null)
                    v.put(WEIGHT, ConvertToWeightNumberFormat(v.get(WEIGHT), v1TenantSettingsResponse));
            }
            dictionary.put(ITEMS ,values);
        }

        if(totalPacks != 0)
            dictionary.put(TOTAL_PACKS, totalPacks);
        else
            dictionary.put(TOTAL_PACKS, null);

    if (breakFlagForWeight || totalWeight.equals(BigDecimal.ZERO)) {
      dictionary.put(TOTAL_WEIGHT, null);
      dictionary.put(UOTW, null);
    } else {
        dictionary.put(TOTAL_WEIGHT, ConvertToWeightNumberFormat(totalWeight, v1TenantSettingsResponse));
        dictionary.put(UOTW, unitOfTotalWeight);
    }

    dictionary.put(PACKAGE_TYPE, cplData.getConsolidationDetails().getPackageType());
    dictionary.put(SPECIAL_INSTRUCTION, cplData.getConsolidationDetails().getSpecialInstructions());
    dictionary.put(TRANSPORT_MODE, cplData.getConsolidationDetails().getTransportMode());

    return dictionary;
    }
}
