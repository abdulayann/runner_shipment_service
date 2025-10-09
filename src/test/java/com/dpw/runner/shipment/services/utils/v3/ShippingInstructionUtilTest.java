package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dao.interfaces.ICommonContainersDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICommonPackagesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShippingInstructionDao;
import com.dpw.runner.shipment.services.entity.ShippingInstruction;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

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

    @InjectMocks
    private ShippingInstructionUtil shippingInstructionUtil;

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
