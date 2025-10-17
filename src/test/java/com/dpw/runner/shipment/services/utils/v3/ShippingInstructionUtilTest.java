package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dao.interfaces.ICommonContainersDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICommonPackagesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShippingInstructionDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.PayerType;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionType;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShippingInstructionUtilTest {
    @Mock
    private ICommonContainersDao commonContainersDao;

    @Mock
    private ICommonPackagesDao commonPackagesDao;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IShippingInstructionDao shippingInstructionDao;

    @Mock
    private IContainerDao containerDao;

    @InjectMocks
    private ShippingInstructionUtil shippingInstructionUtil;

    // ========== VALIDATION TESTS ==========

    @Test
    void testValidateMandatoryFields_AllValid_NoException() {
        ShippingInstruction si = buildCompleteValidSI();
        assertDoesNotThrow(() -> shippingInstructionUtil.validateMandatoryFieldsForSubmitAndAmend(si));
    }

    @Test
    void testValidateMandatoryFields_MissingHeaderFields_ThrowsValidationException() {
        ShippingInstruction si = new ShippingInstruction();
        si.setServiceType(null);
        si.setBlReleaseOffice(null);
        si.setCarrierBookingNo(null);
        si.setShippingInstructionType(null);
        si.setSailingInformation(createValidSailingInfo());
        si.setShipper(createValidParty());
        si.setConsignee(createValidParty());
        si.setNotifyParty(createValidParty());
        si.setContainersList(List.of(createValidContainer()));
        si.setCommonPackagesList(List.of(createValidPackage()));
        si.setFreightDetailList(List.of(createValidFreightDetail()));

        ValidationException ex = assertThrows(ValidationException.class,
                () -> shippingInstructionUtil.validateMandatoryFieldsForSubmitAndAmend(si));

        assertThat(ex.getMessage()).contains("Service Type is mandatory");
        assertThat(ex.getMessage()).contains("BL Release Office is mandatory");
        assertThat(ex.getMessage()).contains("Carrier Booking Number is mandatory");
        assertThat(ex.getMessage()).contains("Original/SeaWaybill");
    }

    @Test
    void testValidateMandatoryFields_MissingSailingInfo_ThrowsValidationException() {
        ShippingInstruction si = new ShippingInstruction();
        si.setServiceType("CY-CY");
        si.setBlReleaseOffice("INMAA");
        si.setCarrierBookingNo("BK12345");
        si.setShippingInstructionType(ShippingInstructionType.ORIGINAL);
        si.setSailingInformation(new SailingInformation());
        si.setShipper(createValidParty());
        si.setConsignee(createValidParty());
        si.setNotifyParty(createValidParty());
        si.setContainersList(List.of(createValidContainer()));
        si.setCommonPackagesList(List.of(createValidPackage()));
        si.setFreightDetailList(List.of(createValidFreightDetail()));

        ValidationException ex = assertThrows(ValidationException.class,
                () -> shippingInstructionUtil.validateMandatoryFieldsForSubmitAndAmend(si));

        assertThat(ex.getMessage()).contains("POL");
        assertThat(ex.getMessage()).contains("POD");
        assertThat(ex.getMessage()).contains("Place of Receipt");
        assertThat(ex.getMessage()).contains("Place of Delivery");
        assertThat(ex.getMessage()).contains("Carrier is mandatory");
        assertThat(ex.getMessage()).contains("Vessel is mandatory");
        assertThat(ex.getMessage()).contains("Voyage is mandatory");
    }

    @Test
    void testValidateMandatoryFields_MissingPartiesAndContainersAndPackages_ThrowsValidationException() {
        ShippingInstruction si = new ShippingInstruction();
        si.setServiceType("CY-CY");
        si.setBlReleaseOffice("INMAA");
        si.setCarrierBookingNo("BK12345");
        si.setShippingInstructionType(ShippingInstructionType.ORIGINAL);
        si.setSailingInformation(createValidSailingInfo());
        si.setShipper(null);
        si.setConsignee(null);
        si.setNotifyParty(null);
        si.setContainersList(Collections.emptyList());
        si.setCommonPackagesList(Collections.emptyList());
        si.setFreightDetailList(List.of(createValidFreightDetail()));

        ValidationException ex = assertThrows(ValidationException.class,
                () -> shippingInstructionUtil.validateMandatoryFieldsForSubmitAndAmend(si));

        assertThat(ex.getMessage()).contains("Shipper is mandatory");
        assertThat(ex.getMessage()).contains("Consignee is mandatory");
        assertThat(ex.getMessage()).contains("Notify Party is mandatory");
        assertThat(ex.getMessage()).contains("At least one Container is mandatory");
        assertThat(ex.getMessage()).contains("At least one Package is mandatory");
    }

    @Test
    void testValidateMandatoryFields_InvalidContainerAndPackageDetails_ThrowsValidationException() {
        ShippingInstruction si = new ShippingInstruction();
        si.setServiceType("CY-CY");
        si.setBlReleaseOffice("INMAA");
        si.setCarrierBookingNo("BK12345");
        si.setShippingInstructionType(ShippingInstructionType.ORIGINAL);
        si.setSailingInformation(createValidSailingInfo());
        si.setShipper(createValidParty());
        si.setConsignee(createValidParty());
        si.setNotifyParty(createValidParty());

        CommonContainers invalidContainer = new CommonContainers();
        invalidContainer.setContainerNo(null);
        invalidContainer.setContainerCode("20GP");
        invalidContainer.setCustomsSealNumber(null);
        invalidContainer.setShipperSealNumber(null);
        invalidContainer.setVeterinarySealNumber(null);
        si.setContainersList(List.of(invalidContainer));

        CommonPackages invalidPackage = new CommonPackages();
        invalidPackage.setPacks(null);
        invalidPackage.setPacksUnit(null);
        invalidPackage.setHsCode(null);
        invalidPackage.setGoodsDescription(null);
        invalidPackage.setGrossWeight(null);
        si.setCommonPackagesList(List.of(invalidPackage));
        si.setFreightDetailList(List.of(createValidFreightDetail()));

        ValidationException ex = assertThrows(ValidationException.class,
                () -> shippingInstructionUtil.validateMandatoryFieldsForSubmitAndAmend(si));

        assertThat(ex.getMessage()).contains("Container #1: Container Number is mandatory");
        assertThat(ex.getMessage()).contains("Container #1: At least one Seal Number is mandatory");
        assertThat(ex.getMessage()).contains("Package #1: Package Count is mandatory");
        assertThat(ex.getMessage()).contains("Package #1: Package Type is mandatory");
        assertThat(ex.getMessage()).contains("Package #1: HS Code is mandatory");
        assertThat(ex.getMessage()).contains("Package #1: Cargo Description is mandatory");
        assertThat(ex.getMessage()).contains("Package #1: Cargo Gross Weight is mandatory");
    }

    @Test
    void testValidateMandatoryFields_MissingFreightDetails_ThrowsValidationException() {
        ShippingInstruction si = buildCompleteValidSI();

        FreightDetail invalidFreight = new FreightDetail();
        invalidFreight.setChargeType(null);
        invalidFreight.setPaymentTerms(null);
        invalidFreight.setPayerLocation(null);
        invalidFreight.setPayerType(null);
        si.setFreightDetailList(List.of(invalidFreight));

        ValidationException ex = assertThrows(ValidationException.class,
                () -> shippingInstructionUtil.validateMandatoryFieldsForSubmitAndAmend(si));

        assertThat(ex.getMessage()).contains("Freight Detail #1: Charge Type is mandatory");
        assertThat(ex.getMessage()).contains("Freight Detail #1: Payment Terms is mandatory");
        assertThat(ex.getMessage()).contains("Freight Detail #1: Payer Location is mandatory");
        assertThat(ex.getMessage()).contains("Freight Detail #1: Payer Type is mandatory");
    }

    // ========== HELPER METHODS ==========

    private ShippingInstruction buildCompleteValidSI() {
        ShippingInstruction si = new ShippingInstruction();
        si.setServiceType("CY-CY");
        si.setBlReleaseOffice("INMAA");
        si.setCarrierBookingNo("BK12345");
        si.setShippingInstructionType(ShippingInstructionType.ORIGINAL);
        si.setSailingInformation(createValidSailingInfo());
        si.setShipper(createValidParty());
        si.setConsignee(createValidParty());
        si.setNotifyParty(createValidParty());
        si.setContainersList(List.of(createValidContainer()));
        si.setCommonPackagesList(List.of(createValidPackage()));
        si.setFreightDetailList(List.of(createValidFreightDetail()));
        return si;
    }

    private SailingInformation createValidSailingInfo() {
        SailingInformation sailing = new SailingInformation();
        sailing.setPol("INMAA");
        sailing.setPod("USLAX");
        sailing.setCarrierReceiptPlace("INMAA");
        sailing.setCarrierDeliveryPlace("USLAX");
        sailing.setCarrier("MSC");
        sailing.setVesselName("MSC OSCAR");
        sailing.setVoyageNo("001W");
        return sailing;
    }

    private Parties createValidParty() {
        Parties party = new Parties();
        party.setOrgCode("ORG123");
        party.setAddressCode("ADD456");
        party.setOrgId("1");
        party.setAddressId("1");
        return party;
    }

    private CommonContainers createValidContainer() {
        CommonContainers container = new CommonContainers();
        container.setContainerNo("MSCU1234567");
        container.setContainerCode("20GP");
        container.setCustomsSealNumber("SEAL123");
        return container;
    }

    private CommonPackages createValidPackage() {
        CommonPackages pkg = new CommonPackages();
        pkg.setPacks(10);
        pkg.setPacksUnit("CARTON");
        pkg.setHsCode("123456");
        pkg.setGoodsDescription("Electronics");
        pkg.setGrossWeight(BigDecimal.valueOf(100));
        pkg.setGrossWeightUnit("KG");
        return pkg;
    }

    private FreightDetail createValidFreightDetail() {
        FreightDetail freight = new FreightDetail();
        freight.setChargeType("FREIGHT");
        freight.setPaymentTerms("PREPAID");
        freight.setPayerLocation("ORIGIN");
        freight.setPayerType(PayerType.SHIPPER);
        return freight;
    }

    // ========== EXISTING EMAIL TESTS ==========

    @Test
    void testAllEmailsPresent() {
        ShippingInstruction instruction = new ShippingInstruction();
        instruction.setInternalEmails("internal1@test.com, internal2@test.com");
        instruction.setCreateByUserEmail("creator@test.com");
        instruction.setSubmitByUserEmail("submitter@test.com");

        List<String> result = shippingInstructionUtil.getSendEmailBaseRequest(instruction);

        assertEquals(4, result.size());
        assertTrue(result.contains("internal1@test.com"));
        assertTrue(result.contains("internal2@test.com"));
        assertTrue(result.contains("creator@test.com"));
        assertTrue(result.contains("submitter@test.com"));
    }

    @Test
    void testNoInternalEmails() {
        ShippingInstruction instruction = new ShippingInstruction();
        instruction.setInternalEmails(null);
        instruction.setCreateByUserEmail("creator@test.com");
        instruction.setSubmitByUserEmail("submitter@test.com");

        List<String> result = shippingInstructionUtil.getSendEmailBaseRequest(instruction);

        assertEquals(2, result.size());
        assertEquals(List.of("creator@test.com", "submitter@test.com"), result);
    }

    @Test
    void testSubmitEmailSameAsCreateEmail() {
        ShippingInstruction instruction = new ShippingInstruction();
        instruction.setInternalEmails("internal@test.com");
        instruction.setCreateByUserEmail("same@test.com");
        instruction.setSubmitByUserEmail("same@test.com");

        List<String> result = shippingInstructionUtil.getSendEmailBaseRequest(instruction);

        assertEquals(2, result.size());
        assertTrue(result.contains("internal@test.com"));
        assertTrue(result.contains("same@test.com"));
    }

    @Test
    void testEmailsWithSpacesAndEmptyStrings() {
        ShippingInstruction instruction = new ShippingInstruction();
        instruction.setInternalEmails("  internal1@test.com  ,   ");
        instruction.setCreateByUserEmail("   creator@test.com ");
        instruction.setSubmitByUserEmail("  submitter@test.com   ");

        List<String> result = shippingInstructionUtil.getSendEmailBaseRequest(instruction);

        assertEquals(3, result.size());
        assertEquals(List.of("internal1@test.com", "creator@test.com", "submitter@test.com"), result);
    }

    @Test
    void testNullValuesEverywhere() {
        ShippingInstruction instruction = new ShippingInstruction();
        instruction.setInternalEmails(null);
        instruction.setCreateByUserEmail(null);
        instruction.setSubmitByUserEmail(null);

        List<String> result = shippingInstructionUtil.getSendEmailBaseRequest(instruction);

        assertTrue(result.isEmpty());
    }

    @Test
    void testDuplicateEmailsShouldBeRemoved() {
        ShippingInstruction instruction = new ShippingInstruction();
        instruction.setInternalEmails("dup@test.com,dup@test.com");
        instruction.setCreateByUserEmail("dup@test.com");
        instruction.setSubmitByUserEmail("dup@test.com");

        List<String> result = shippingInstructionUtil.getSendEmailBaseRequest(instruction);

        assertEquals(1, result.size());
        assertEquals("dup@test.com", result.get(0));
    }
}
