package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.Models.ConsolidatedPackingListModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PackingModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.fasterxml.jackson.core.type.TypeReference;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.*;
import static java.lang.Long.sum;

@Component
public class ConsolidatedPackingListReport extends IReport {

    @Autowired
    JsonHelper jsonHelper;


/**
*
   * @param id
   * @return
*/
    @Override
    public Map<String, Object> getData(Long id){
        ConsolidatedPackingListModel cplData = (ConsolidatedPackingListModel) getDocumentModel(id);
        return populateDictionary(cplData);
    }

/**
*
   * @param id
   * @return
*/
    @Override
    IDocumentModel getDocumentModel(Long id) {
        ConsolidatedPackingListModel cplData = new ConsolidatedPackingListModel();
        cplData.setConsolidationDetails(getConsolidation(id));
        cplData.setTenant(getTenant(TenantContext.getCurrentTenant()));
        return cplData;
    }

/**
*
   * @param documentModel
   * @return
*/
    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        ConsolidatedPackingListModel cplData = (ConsolidatedPackingListModel) documentModel;
        String json = jsonHelper.convertToJson(cplData.getConsolidationDetails());
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);


        List<String> exporter = getOrgAddressWithPhoneEmail(jsonHelper.convertValue(
                cplData.getConsolidationDetails().getSendingAgent(), PartiesModel.class
        ));
        if(cplData.getConsolidationDetails().getSendingAgent() != null){
            //sending agent full name ?
//            exporter.add(0, cplData.getConsolidationDetails().getSendingAgent());
        }

        List<String> consignee = getOrgAddressWithPhoneEmail(jsonHelper.convertValue(
                cplData.getConsolidationDetails().getReceivingAgent(), PartiesModel.class
        ));
        if(cplData.getConsolidationDetails().getReceivingAgent() != null){
            //receiving agent full name ?
//            exporter.add(0, cplData.getConsolidationDetails().getReceivingAgent());
        }

        addTenantDetails(dictionary, cplData.getTenant());
//        dictionary["Tenant"] = ReportHelper.getListOfStrings(cplData?.tenant?.TenantName, cplData?.tenant?.Address1, cplData?.tenant?.Address2, cplData?.tenant?.City, cplData?.tenant?.State, cplData?.tenant?.ZipPostCode, cplData?.tenant?.Country, cplData?.tenant?.Email, cplData?.tenant?.WebsiteUrl, cplData?.tenant?.Phone);
        dictionary.put(EXPORTER, exporter);
        dictionary.put(CONSIGNEE, consignee);

        if (cplData.getConsolidationDetails().getIsSendingAgentFreeTextAddress()) {
            dictionary.put(EXPORT_AGENT_FREETEXT, getAddressList(cplData.getConsolidationDetails().getReceivingAgentFreeTextAddress()));
        } else {
            dictionary.put(EXPORT_AGENT_FREETEXT, exporter);
        }

        if (cplData.getConsolidationDetails().getIsReceivingAgentFreeTextAddress()) {
            dictionary.put(IMPORT_AGENT_FREETEXT, getAddressList(cplData.getConsolidationDetails().getReceivingAgentFreeTextAddress()));
        } else {
            dictionary.put(IMPORT_AGENT_FREETEXT, consignee);
        }

        // line 99 - 144
        var exportOrgData = cplData.getConsolidationDetails().getSendingAgent().getOrgData();
        if(exportOrgData != null){
            dictionary.put(EXPORTER_TAX_ID, exportOrgData.get(TENANT_VATREGNUMBER));
        }else {
            dictionary.put(EXPORTER_TAX_ID, null);
        }

        var consigneeOrgData = cplData.getConsolidationDetails().getReceivingAgent().getOrgData();
        if(consigneeOrgData != null){
            dictionary.put(CONSIGNEE_TAX_ID, consigneeOrgData.get(TENANT_VATREGNUMBER));
        }else {
            dictionary.put(CONSIGNEE_TAX_ID, null);
        }

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

        List<PackingModel> packingList = cplData.getConsolidationDetails().getPackingList();
        long totalPacks = 0L;
        BigDecimal totalWeight = BigDecimal.ZERO;
        String unitOfTotalWeight = null;
        boolean breakFlagForWeight = false;
        List<ShipmentModel> shipments = cplData.getConsolidationDetails().getShipmentsList();

        for (var shipment : shipments){
            packingList.addAll(shipment.getPackingList());
        }

        if(packingList != null) {
            String packingJson = jsonHelper.convertToJson(packingList);
            var values = jsonHelper.convertValue(packingJson, new TypeReference<List<Map<String, Object>>>() {});
            for(var v : values){
                totalPacks = sum(totalPacks, Long.parseLong(v.get(PACKS).toString()));
                if(!breakFlagForWeight && v.get(WEIGHT) != null && v.get(WEIGHT_UNIT) !=null)
                {
                    if(unitOfTotalWeight == null) {
                        unitOfTotalWeight = v.get(WEIGHT_UNIT).toString();
                        totalWeight = totalWeight.add(BigDecimal.valueOf((double) v.get(WEIGHT)));
                    }
                    else if(!unitOfTotalWeight.equals(v.get(WEIGHT_UNIT).toString())) {
                        totalWeight = BigDecimal.ZERO;
                        breakFlagForWeight = true;
                    }
                    else
                        totalWeight = totalWeight.add(BigDecimal.valueOf((double) v.get(WEIGHT)));
                }
                if(v.get(WEIGHT) != null)
                    v.put(WEIGHT, twoDecimalPlacesFormat(v.get(WEIGHT).toString()));
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
        dictionary.put(TOTAL_WEIGHT, twoDecimalPlacesFormat(totalWeight.toString()));
        dictionary.put(UOTW, unitOfTotalWeight);
    }

    dictionary.put(PACKAGE_TYPE, cplData.getConsolidationDetails().getPackageType());
    dictionary.put(SPECIAL_INSTRUCTION, cplData.getConsolidationDetails().getSpecialInstructions());
    dictionary.put(TRANSPORT_MODE, cplData.getConsolidationDetails().getTransportMode());

    return dictionary;
    }
}
