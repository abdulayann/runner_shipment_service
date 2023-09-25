package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.Models.CommercialInvoiceModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.fasterxml.jackson.core.type.TypeReference;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static com.dpw.runner.shipment.services.utils.StringUtility.getRandomString;

@Component
public class CommercialInvoiceReport extends IReport{

    @Autowired
    JsonHelper jsonHelper;
    @Autowired
    IShipmentRepository shipmentRepository;

    private CommercialInvoiceModel commercialInvoiceModel;

    public Long shipmentId;


/**
*
   * @param id
   * @return
*/
    @Override
    public Map<String, Object> getData(Long id) {
        Optional<ShipmentDetails> shipment = shipmentRepository.findById(shipmentId);
        if(shipment.isEmpty()){
            throw new RunnerException(DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG);
        }

        Integer tenant = TenantContext.getCurrentTenant();
        String commercialInvoiceNumber = getRandomString(7);

        // Package Count, Total amount pending
        // ShipmentProducts in v1 contains this information

        commercialInvoiceModel.setShipmentDetails(shipment.get());
        commercialInvoiceModel.setTenant(tenant);
        commercialInvoiceModel.setCommercialInvoiceNumber(commercialInvoiceNumber);

        return jsonHelper.convertValue(commercialInvoiceModel, new TypeReference<Map<String, Object>>() {});
    }

/**
*
   * @param id
   * @return
*/
    @Override
    public IDocumentModel getDocumentModel(Long id) {
        return CommercialInvoiceModel.builder()
                .commercialInvoiceNumber("something something")
                .packageSummary(List.of("package"))
                .totalAmount(BigDecimal.ZERO)
                .build();
    }

/**
*
   * @param documentModel
   * @return
*/
    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        return null;
    }
}
