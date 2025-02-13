package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.ConsolidationManifestPrintModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PackingModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.nimbusds.jose.util.Pair;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.*;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.EMPTY_STRING;
import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;

@Component
public class ContainerManifestPrint extends IReport {
    @Autowired
    private JsonHelper jsonHelper;

    @Override
    public Map<String, Object> getData(Long id) {
        ConsolidationManifestPrintModel manifestPrintModel = (ConsolidationManifestPrintModel) getDocumentModel(id);
        return populateDictionary(manifestPrintModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) {
        ConsolidationManifestPrintModel manifestPrintModel = new ConsolidationManifestPrintModel();
        ConsolidationModel consol = getConsolidation(id);
        manifestPrintModel.setConsol(consol);
        manifestPrintModel.setShipments(consol.getShipmentsList());
        return manifestPrintModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        ConsolidationManifestPrintModel manifestPrintModel = (ConsolidationManifestPrintModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        for (var shipmentDetails : manifestPrintModel.getShipments()) {
            populateShipmentFields(shipmentDetails, dictionary);
        }
        ConsolidationModel consol = manifestPrintModel.getConsol();

        populateConsolidationFields(consol, dictionary);

        List<PackingModel> packings = GetAllShipmentsPacks(List.of(manifestPrintModel.getShipments().toArray(new ShipmentModel[0])));
        Pair<BigDecimal, String> weightAndUnit = GetTotalWeight(packings);
        Pair<BigDecimal, String> volumeAndUnit = GetTotalVolume(packings);


        if (consol.getAchievedQuantities() != null && consol.getAchievedQuantities().getConsolidatedWeight() != null) {
            dictionary.put(ReportConstants.PWEIGHT_UNIT, ConvertToWeightNumberFormat(consol.getAchievedQuantities().getConsolidatedWeight(), v1TenantSettingsResponse) + " " + consol.getAchievedQuantities().getConsolidatedWeightUnit());
        } else {
            dictionary.put(ReportConstants.PWEIGHT_UNIT, EMPTY_STRING);
        }

        if (consol.getAchievedQuantities() != null && consol.getAchievedQuantities().getConsolidatedVolume() != null) {
            dictionary.put(ReportConstants.PVOLUME_UNIT, ConvertToVolumeNumberFormat(consol.getAchievedQuantities().getConsolidatedVolume(), v1TenantSettingsResponse) + " " + consol.getAchievedQuantities().getConsolidatedVolumeUnit());
        } else {
            dictionary.put(ReportConstants.PVOLUME_UNIT, EMPTY_STRING);
        }

        if (consol.getAchievedQuantities() != null && consol.getAchievedQuantities().getConsolidationChargeQuantity() != null) {
            dictionary.put(ReportConstants.PCHARGE_UNIT, consol.getAchievedQuantities().getConsolidationChargeQuantity() + " " + consol.getAchievedQuantities().getConsolidationChargeQuantityUnit());
        } else {
            dictionary.put(ReportConstants.PCHARGE_UNIT, EMPTY_STRING);
        }

        var containersList = consol.getContainersList();
        long totalPackages = 0L;
        if (containersList != null && !containersList.isEmpty()) {
            totalPackages =
                    containersList.stream()
                            .filter(Objects::nonNull) // Filter out null values
                            .map(c -> IsStringNullOrEmpty(c.getPacks()) ? 0 : Long.parseLong(c.getPacks()))
                            .reduce(Long::sum)
                            .orElse(0L); // Default value if the stream is empty
        }

        if (totalPackages == 0) {
            dictionary.put(ReportConstants.CONSOL_SHIPMENT_TOTAL_PACKAGES, StringUtils.EMPTY);
        } else {
            dictionary.put(ReportConstants.CONSOL_SHIPMENT_TOTAL_PACKAGES, totalPackages);
        }

        String customReferenceNumber = "";
        if (consol.getId() != null) {
            customReferenceNumber = consol.getReferenceNumber();
        }
        dictionary.put(ReportConstants.CUSTOMS_ENTRY_NUMBER, customReferenceNumber);

        var exportAgentAddress = ReportHelper.getOrgAddress(consol.getSendingAgent());
        var importAgentAddress = ReportHelper.getOrgAddress(consol.getReceivingAgent());
        var ctoAddress = consol.getArrivalDetails() == null ? new ArrayList() : ReportHelper.getOrgAddress(consol.getArrivalDetails().getCTOId());
        List<String> exportAgentFreeTextAddress;
        List<String> importAgentFreeTextAddress;

        if (consol.getIsSendingAgentFreeTextAddress() != null && consol.getIsSendingAgentFreeTextAddress()) {
            exportAgentFreeTextAddress = ReportHelper.getAddressList(consol.getSendingAgentFreeTextAddress());
        } else {
            exportAgentFreeTextAddress = exportAgentAddress;
        }

        if (consol.getIsReceivingAgentFreeTextAddress() != null && consol.getIsReceivingAgentFreeTextAddress()) {
            importAgentFreeTextAddress = ReportHelper.getAddressList(consol.getReceivingAgentFreeTextAddress());
        } else {
            importAgentFreeTextAddress = importAgentAddress;
        }

        if (consol.getShipmentType().equalsIgnoreCase("EXP")) {
            dictionary.put(ReportConstants.EXPORT_AGENT, exportAgentAddress);
            dictionary.put(ReportConstants.IMPORT_AGENT, importAgentAddress);
            dictionary.put(ReportConstants.EXPORT_AGENT_FREETEXT, exportAgentFreeTextAddress);
            dictionary.put(ReportConstants.IMPORT_AGENT_FREETEXT, importAgentFreeTextAddress);
            UnlocationsResponse arrival = consol.getArrivalDetails() == null ? null : getUNLocRow(consol.getArrivalDetails().getLastForeignPort());
            if (arrival != null)
                dictionary.put(ReportConstants.LAST_FOREIGN_PORT_NAME, ReportHelper.getCityCountry(arrival.getNameWoDiacritics(), arrival.getCountry()));

        } else if (consol.getShipmentType().equalsIgnoreCase("IMP")) {
            dictionary.put(ReportConstants.EXPORT_AGENT, importAgentAddress);
            dictionary.put(ReportConstants.IMPORT_AGENT, exportAgentAddress);
            dictionary.put(ReportConstants.EXPORT_AGENT_FREETEXT, importAgentFreeTextAddress);
            dictionary.put(ReportConstants.IMPORT_AGENT_FREETEXT, exportAgentFreeTextAddress);
            UnlocationsResponse depart = consol.getDepartureDetails() == null ? null : getUNLocRow(consol.getDepartureDetails().getLastForeignPort());
            if (depart != null) {
                dictionary.put(ReportConstants.LAST_FOREIGN_PORT_NAME, ReportHelper.getCityCountry(depart.getNameWoDiacritics(), depart.getCountry()));
            }
        } else {
            dictionary.put(ReportConstants.LAST_FOREIGN_PORT_NAME, StringUtils.EMPTY);
        }

        dictionary.put(ReportConstants.CTO_ADDRESS, ctoAddress);
        dictionary.put(ReportConstants.TOTAL_WEIGHT, ConvertToWeightNumberFormat(weightAndUnit.getLeft(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.TOTAL_WEIGHT_UNIT, weightAndUnit.getRight());
        dictionary.put(ReportConstants.TOTAL_VOLUME, ConvertToVolumeNumberFormat(volumeAndUnit.getLeft(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.TOTAL_VOLUME_UNIT, volumeAndUnit.getRight());
        var user = UserContext.getUser();
        dictionary.put(ReportConstants.UN, user.getUsername());
        dictionary.put(ReportConstants.COMMON_CONTAINERS, consol.getContainersList());
        dictionary.put(ReportConstants.SHIPMENTS, consol.getShipmentsList());
        dictionary.put(ReportConstants.CONTAINER_COUNT_BY_CODE,
                getCountByContainerTypeCode(consol.getContainersList().stream().map(c -> getShipmentContainer(c)).toList()));

        if (consol.getCarrierDetails() != null) {
            UnlocationsResponse originUnloc = null;
            UnlocationsResponse destinationUnloc = null;
            if (consol.getCarrierDetails().getOriginPort() != null) {
                originUnloc = getUNLocRow(consol.getCarrierDetails().getOriginPort());
            }
            if (consol.getCarrierDetails().getDestinationPort() != null) {

            }
            destinationUnloc = getUNLocRow(consol.getCarrierDetails().getDestinationPort());
            if (originUnloc != null)
                dictionary.put(ReportConstants.ORIGIN_CODE, ReportHelper.getCityCountry(originUnloc.getNameWoDiacritics(), originUnloc.getCountry()));
            if (destinationUnloc != null)
                dictionary.put(ReportConstants.DESTINATION_CODE, ReportHelper.getCityCountry(destinationUnloc.getNameWoDiacritics(), destinationUnloc.getCountry()));
        }

        updateShipmentWeightAndPack(dictionary, v1TenantSettingsResponse);
        return dictionary;
    }
}
