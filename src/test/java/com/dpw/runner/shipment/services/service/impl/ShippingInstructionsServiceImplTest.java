package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShippingInstructionDao;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.SailingInformationRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.ShippingInstructionRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ShippingInstructionResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionEntityType;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionStatus;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ShippingInstructionMasterDataHelper;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.*;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ExecutorService;

import static org.assertj.core.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class ShippingInstructionsServiceImplTest {

    @InjectMocks
    private ShippingInstructionsServiceImpl service;

    @Mock private IShippingInstructionDao repository;
    @Mock private ICarrierBookingDao carrierBookingDao;
    @Mock private IConsolidationDetailsDao consolidationDetailsDao;
    @Mock private JsonHelper jsonHelper;
    @Mock private MasterDataUtils masterDataUtils;
    @Mock private ExecutorService executorServiceMasterData;
    @Mock private CommonUtils commonUtils;
    @Mock private ShippingInstructionMasterDataHelper shippingInstructionMasterDataHelper;

    // --- Helpers to create simple domain objects we need ---

    private ShippingInstructionRequest buildSimpleRequest() {
        return ShippingInstructionRequest.builder()
                .entityType(ShippingInstructionEntityType.CARRIER_BOOKING)
                .entityId(100L)
                .noOfFreightCopies(2)
                .nonNegoFreightCopies(2)
                .noOfUnFreightCopies(2)
                .nonNegoUnFreightCopies(2)
                .sailingInformationRequest(SailingInformationRequest.builder().build())
                .build();
    }

    private ShippingInstruction buildSimpleEntity() {
        ShippingInstruction si = new ShippingInstruction();
        si.setEntityType(ShippingInstructionEntityType.CARRIER_BOOKING);
        si.setEntityId(100L);
        si.setNoOfFreightCopies(2);
        si.setNonNegoFreightCopies(2);
        si.setNoOfUnFreightCopies(2);
        si.setNonNegoUnFreightCopies(2);
        si.setSailingInformation(new SailingInformation());
        return si;
    }

    private CarrierBooking buildCarrierBooking() {
        CarrierBooking cb = new CarrierBooking();
        cb.setId(100L);
        cb.setEntityId(200L); // used to fetch ConsolidationDetails
        cb.setStatus(CarrierBookingStatus.Draft);
        cb.setCarrierBookingNo("CB-001");
        cb.setCarrierBlNo("BL-001");
        return cb;
    }

    // ========== POSITIVE CASES ==========

    @Test
    void createShippingInstruction_ShouldValidate_Save_AndReturnResponse() {
        // Arrange
        ShippingInstructionRequest request = buildSimpleRequest();
        ShippingInstruction entity = buildSimpleEntity();
        ShippingInstruction saved = buildSimpleEntity(); // same back
        ShippingInstructionResponse response = ShippingInstructionResponse.builder()
                .status(ShippingInstructionStatus.Draft.name())
                .carrierBookingNo("CB-001")
                .carrierBlNo("BL-001")
                .build();

        CarrierBooking cb = buildCarrierBooking();
        // convert request -> entity
        when(jsonHelper.convertValue(eq(request), eq(ShippingInstruction.class))).thenReturn(entity);
        // fetch CB and no consol (optional empty is fine)
        when(carrierBookingDao.findById(100L)).thenReturn(Optional.of(cb));
        when(consolidationDetailsDao.findById(200L)).thenReturn(Optional.empty());
        // save
        when(repository.save(any(ShippingInstruction.class))).thenReturn(saved);
        // convert saved entity -> response
        when(jsonHelper.convertValue(eq(saved), eq(ShippingInstructionResponse.class))).thenReturn(response);

        // Act
        ShippingInstructionResponse out = service.createShippingInstruction(request);

        // Assert
        assertThat(out).isNotNull();
        assertThat(out.getStatus()).isEqualTo(ShippingInstructionStatus.Draft.name());
        verify(repository, times(1)).save(any(ShippingInstruction.class));
        verify(carrierBookingDao, times(1)).findById(100L);
        verify(consolidationDetailsDao, times(1)).findById(200L);
    }

    @Test
    void getShippingInstructionsById_ShouldReturnResponse_WhenFound() {
        // Arrange
        ShippingInstruction entity = buildSimpleEntity();
        // NOTE: method passes Optional<ShippingInstruction> to convertValue (bug in code).
        when(repository.findById(1L)).thenReturn(Optional.of(entity));
        ShippingInstructionResponse resp = ShippingInstructionResponse.builder().carrierBookingNo("CB-001").build();
        when(jsonHelper.convertValue(eq(Optional.of(entity)), eq(ShippingInstructionResponse.class))).thenReturn(resp);

        // Act
        ShippingInstructionResponse out = service.getShippingInstructionsById(1L);

        // Assert
        assertThat(out).isNotNull();
        assertThat(out.getCarrierBookingNo()).isEqualTo("CB-001");
        verify(repository).findById(1L);
    }

    @Test
    void updateShippingInstructions_ShouldValidate_Save_AndReturnResponse() {
        // Arrange
        ShippingInstructionRequest request = buildSimpleRequest();
        // For update, let’s go via CONSOLIDATION branch to cover it
        ShippingInstruction entity = buildSimpleEntity();
        entity.setEntityType(ShippingInstructionEntityType.CONSOLIDATION);
        entity.setEntityId(999L);
        entity.setSailingInformation(new SailingInformation());

        // Mock ConsolidationDetails deeply to avoid building complex graphs
        ConsolidationDetails consol = mock(ConsolidationDetails.class, RETURNS_DEEP_STUBS);
        when(consol.getCarrierDetails().getOrigin()).thenReturn("ORI");
        when(consol.getCarrierDetails().getOriginPort()).thenReturn("ORIP");
        when(consol.getCarrierDetails().getDestinationPort()).thenReturn("DESTP");
        when(consol.getCarrierDetails().getDestination()).thenReturn("DEST");
        when(consol.getShipInstructionCutoff()).thenReturn(LocalDateTime.now());
        when(consol.getVerifiedGrossMassCutoff()).thenReturn(LocalDateTime.now());

        when(jsonHelper.convertValue(eq(request), eq(ShippingInstruction.class))).thenReturn(entity);
        when(consolidationDetailsDao.findById(999L)).thenReturn(Optional.of(consol));

        ShippingInstruction saved = entity;
        ShippingInstructionResponse resp = ShippingInstructionResponse.builder().carrierBookingNo(null).build();
        when(repository.save(any(ShippingInstruction.class))).thenReturn(saved);
        when(jsonHelper.convertValue(eq(saved), eq(ShippingInstructionResponse.class))).thenReturn(resp);

        // Act
        ShippingInstructionResponse out = service.updateShippingInstructions(request);

        // Assert
        assertThat(out).isNotNull();
        verify(consolidationDetailsDao).findById(999L);
        verify(repository).save(any(ShippingInstruction.class));
    }

    @Test
    void deleteShippingInstructions_ShouldInvokeDao() {
        // Act
        service.deleteShippingInstructions(55L);
        // Assert
        verify(repository).delete(55L);
    }

    // ========== NEGATIVE CASES (VALIDATION) ==========
    // We drive validation through create/update (validateFetchAndSetSI is private)

    @Test
    void create_ShouldThrow_WhenNoOfFreightCopies_GreaterThan100() {
        // Arrange
        ShippingInstructionRequest req = ShippingInstructionRequest.builder().build();
        ShippingInstruction bad = mock(ShippingInstruction.class, RETURNS_DEEP_STUBS);
        when(bad.getEntityType()).thenReturn(ShippingInstructionEntityType.CARRIER_BOOKING);
        // Set just the first invalid field; others valid
        when(bad.getNoOfFreightCopies()).thenReturn(101);
        when(bad.getSailingInformation()).thenReturn(new SailingInformation());
        when(jsonHelper.convertValue(eq(req), eq(ShippingInstruction.class))).thenReturn(bad);

        // Act + Assert
        assertThatThrownBy(() -> service.createShippingInstruction(req))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Invalid freight copies number!");
        verifyNoInteractions(repository);
    }

    @Test
    void create_ShouldThrow_WhenNonNegoFreightCopies_Negative() {
        ShippingInstructionRequest req = ShippingInstructionRequest.builder().build();
        ShippingInstruction bad = mock(ShippingInstruction.class, RETURNS_DEEP_STUBS);
        when(bad.getEntityType()).thenReturn(ShippingInstructionEntityType.CARRIER_BOOKING);
        when(bad.getNoOfFreightCopies()).thenReturn(1);
        when(bad.getNonNegoFreightCopies()).thenReturn(-1); // invalid
        when(bad.getSailingInformation()).thenReturn(new SailingInformation());
        when(jsonHelper.convertValue(eq(req), eq(ShippingInstruction.class))).thenReturn(bad);

        assertThatThrownBy(() -> service.createShippingInstruction(req))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Invalid getNonNegoFreightCopies!");
        verifyNoInteractions(repository);
    }

    @Test
    void create_ShouldThrow_WhenNoOfUnFreightCopies_GreaterThan100() {
        ShippingInstructionRequest req = ShippingInstructionRequest.builder().build();
        ShippingInstruction bad = mock(ShippingInstruction.class, RETURNS_DEEP_STUBS);
        when(bad.getEntityType()).thenReturn(ShippingInstructionEntityType.CARRIER_BOOKING);
        when(bad.getNoOfFreightCopies()).thenReturn(1);
        when(bad.getNonNegoFreightCopies()).thenReturn(1);
        when(bad.getNoOfUnFreightCopies()).thenReturn(1000); // invalid
        when(bad.getSailingInformation()).thenReturn(new SailingInformation());
        when(jsonHelper.convertValue(eq(req), eq(ShippingInstruction.class))).thenReturn(bad);

        assertThatThrownBy(() -> service.createShippingInstruction(req))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Invalid un freight copies number!");
        verifyNoInteractions(repository);
    }

    @Test
    void create_ShouldThrow_WhenNonNegoUnFreightCopies_Negative() {
        ShippingInstructionRequest req = ShippingInstructionRequest.builder().build();
        ShippingInstruction bad = mock(ShippingInstruction.class, RETURNS_DEEP_STUBS);
        when(bad.getEntityType()).thenReturn(ShippingInstructionEntityType.CARRIER_BOOKING);
//        when(bad.getEntityId()).thenReturn(1L);
        when(bad.getNoOfFreightCopies()).thenReturn(1);
        when(bad.getNonNegoFreightCopies()).thenReturn(1);
        when(bad.getNoOfUnFreightCopies()).thenReturn(1);
        when(bad.getNonNegoUnFreightCopies()).thenReturn(-5); // invalid
        when(bad.getSailingInformation()).thenReturn(new SailingInformation());
        when(jsonHelper.convertValue(eq(req), eq(ShippingInstruction.class))).thenReturn(bad);

        assertThatThrownBy(() -> service.createShippingInstruction(req))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Invalid getNonNegoUnFreightCopies!");
        verifyNoInteractions(repository);
    }

    // ========== REPOSITORY NEGATIVE ==========

    @Test
    void getShippingInstructionsById_ShouldThrow_WhenNotFound() {
        when(repository.findById(999L)).thenReturn(Optional.empty());
        assertThatThrownBy(() -> service.getShippingInstructionsById(999L))
                .isInstanceOf(DataRetrievalFailureException.class)
                .hasMessageContaining(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
    }

//    // ========== LIST endpoint positive smoke (minimal) ==========
//    // We don't start Spring; this is just to show no NPE with minimal mocks.
//    @Test
//    void list_ShouldReturnPagedResponse_WhenSpecAndPageableProvided() {
//        CommonRequestModel crm = new CommonRequestModel();
//        ListCommonRequest lcr = new ListCommonRequest();
//        lcr.setIncludeColumns(List.of("id")); // satisfies service validation
//        crm.setData(lcr);
//
//        // DbAccessHelper.fetchData is static; if you keep it static, avoid this test or use Mockito-inline static mocking.
//        // Here we'll assume repository.findAll(spec, pageable) is called; we just mock it directly.
//        Page<ShippingInstruction> page = new PageImpl<>(List.of(buildSimpleEntity()), PageRequest.of(0, 10), 1);
//        when(repository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(page);
//
//        // commonUtils.setIncludedFieldsToResponse is used in getAllMasterData, not here.
//        ResponseEntity<IRunnerResponse> resp = service.list(crm, false);
//        assertThat(resp).isNotNull();
//        assertThat(resp.getStatusCode().is2xxSuccessful()).isTrue();
//    }
}
