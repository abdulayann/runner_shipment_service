package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.Models.HawbModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.enums.ModuleValidationFieldType;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.ReportException;
import com.dpw.runner.shipment.services.service.impl.ConsolidationService;
import com.dpw.runner.shipment.services.service.impl.ShipmentService;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.apache.commons.lang3.ObjectUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MawbReport extends IReport {

    private HawbReport hawbReport;
    private V1ServiceUtil v1ServiceUtil;
    private ModelMapper modelMapper;
    private ShipmentService shipmentService;
    private ConsolidationService consolidationService;

    public boolean isDMawb;
    private V1TenantSettingsResponse tenantSettings;

    @Autowired
    public MawbReport(HawbReport hawbReport, V1ServiceUtil v1ServiceUtil,
            ModelMapper modelMapper, ShipmentService shipmentService, ConsolidationService consolidationService) {
        this.hawbReport = hawbReport;
        this.v1ServiceUtil = v1ServiceUtil;
        this.modelMapper = modelMapper;
        this.shipmentService = shipmentService;
        this.consolidationService = consolidationService;
    }

    @Override
    public Map<String, Object> getData(Long id) {
        validatePrinting(id);
        HawbModel mawbModel = (HawbModel) getDocumentModel(id);
        return populateDictionary(mawbModel);
    }

    public void validatePrinting(Long id) {

        if (tenantSettings == null) {
            tenantSettings = getCurrentTenantSettings();
        }

        if (Boolean.TRUE.equals(tenantSettings.getIsModuleValidationEnabled())) {

            List<ModuleValidationFieldType> missingFields = new ArrayList<>();
            if (!isDMawb) {
                ConsolidationDetails consolidation = getConsolidationsById(id);
                if(consolidation==null) {
                    throw new ReportException("No consolidation found with id: " + id);
                }

                if (Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(consolidation.getTransportMode())
                        && Constants.DIRECTION_EXP.equalsIgnoreCase(consolidation.getShipmentType())
                        && Constants.CARGO_TYPE_LSE.equalsIgnoreCase(consolidation.getContainerCategory())
                        && Constants.CONSOLIDATION_TYPE_DRT.equalsIgnoreCase(consolidation.getConsolidationType())) {

                    consolidationService.validateCarrierDetails(consolidation, missingFields);
                    consolidationService.validateContainerDetails(consolidation, missingFields);

                }
            } else {
                ShipmentDetails shipment = getShipmentDetails(id);
                if(shipment==null){
                    throw new ReportException("No shipment found with id: " + id);
                }

                if (Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(shipment.getTransportMode())
                        && Constants.DIRECTION_EXP.equalsIgnoreCase(shipment.getDirection())
                        && Constants.CARGO_TYPE_LSE.equalsIgnoreCase(shipment.getShipmentType())
                        && Constants.SHIPMENT_TYPE_DRT.equalsIgnoreCase(shipment.getJobType())) {

                    shipmentService.validateCarrierDetails(shipment, missingFields);
                    shipmentService.validateContainerDetails(shipment, missingFields);

                }
            }

            if (ObjectUtils.isNotEmpty(missingFields)) {
                String missingFieldsDescription = missingFields.stream()
                        .map(ModuleValidationFieldType::getDescription)
                        .collect(Collectors.joining(" | "));
                throw new ReportException(missingFieldsDescription);
            }
        }
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        HawbModel hawbModel = new HawbModel();
        if (!isDMawb) {
            hawbModel.usersDto = UserContext.getUser();
            hawbModel.setConsolidationDetails(getConsolidation(id));
            validateAirDGCheckConsolidations(hawbModel.getConsolidationDetails());
            String entityType = "MAWB";
            hawbModel.setMawb(getMawb(hawbModel.getConsolidationDetails().getId()));
            hawbModel.awb = hawbModel.getMawb();
            hawbModel.setEntityType(entityType);
        } else {
            hawbModel.usersDto = UserContext.getUser();
            hawbModel.shipmentDetails = getShipment(id);
            validateAirDGCheckShipments(hawbModel.shipmentDetails);
            String entityType = "MAWB";
            if(hawbModel.shipmentDetails != null && hawbModel.shipmentDetails.getConsolidationList() != null && !hawbModel.shipmentDetails.getConsolidationList().isEmpty())
            {
                hawbModel.setConsolidationDetails(hawbModel.shipmentDetails.getConsolidationList().get(0));
                hawbModel.setMawb(getMawb(hawbModel.getConsolidationDetails().getId()));
                hawbModel.awb = hawbModel.getMawb();
            }
            if(hawbModel.getMawb() == null){
                hawbModel.awb = getHawb(id);
                entityType = "DMAWB";
            }
            hawbModel.setEntityType(entityType);
        }

        return hawbModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        HawbModel model = (HawbModel) documentModel;
        var dictionary =  hawbReport.populateDictionary(documentModel);
        if(model.getConsolidationDetails() != null) {
            populateRaKcDataConsolidation(dictionary, model.getConsolidationDetails());
        }
        return dictionary;
    }
}
