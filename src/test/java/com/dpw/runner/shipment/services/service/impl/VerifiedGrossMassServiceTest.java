package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.impl.BridgeServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.VerifiedGrossMassConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.impl.CarrierBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITransactionHistoryDao;
import com.dpw.runner.shipment.services.dao.interfaces.IVerifiedGrossMassDao;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.SubmitAmendInttraRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.VerifiedGrossMassRequest;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassBulkUpdateRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.VerifiedGrossMass;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.entity.enums.OperationType;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionStatus;
import com.dpw.runner.shipment.services.entity.enums.VerifiedGrossMassStatus;
import com.dpw.runner.shipment.services.entity.enums.WeightDeterminationMethodType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.helpers.VerifiedGrossMassMasterDataHelper;
import com.dpw.runner.shipment.services.kafka.dto.inttra.VgmEventDto;
import com.dpw.runner.shipment.services.notification.request.SendEmailBaseRequest;
import com.dpw.runner.shipment.services.notification.response.NotificationServiceResponse;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.projection.CarrierBookingInfoProjection;
import com.dpw.runner.shipment.services.repository.interfaces.ICommonContainersRepository;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.v3.CarrierBookingInttraUtil;
import com.dpw.runner.shipment.services.utils.v3.VerifiedGrossMassUtil;
import com.dpw.runner.shipment.services.utils.v3.VerifiedGrossMassValidationUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ExecutorService;

import static com.dpw.runner.shipment.services.commons.constants.VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_EMAIL_TEMPLATE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class VerifiedGrossMassServiceTest {

    @Mock
    private IVerifiedGrossMassDao verifiedGrossMassDao;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private CarrierBookingDao carrierBookingDao;

    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Mock
    private CommonUtils commonUtils;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private ExecutorService executorServiceMasterData;

    @Mock
    private VerifiedGrossMassUtil verifiedGrossMassUtil;

    @Mock
    private CarrierBookingInttraUtil carrierBookingInttraUtil;

    @Mock
    private BridgeServiceAdapter bridgeServiceAdapter;

    @Mock
    private VerifiedGrossMassMasterDataHelper verifiedGrossMassMasterDataHelper;

    @Mock
    private VerifiedGrossMassValidationUtil verifiedGrossMassValidationUtil;

    @Mock
    private ICommonContainersRepository commonContainersRepository;

    @Mock
    private INotificationService notificationService;
    @Mock
    private ITransactionHistoryDao transactionHistoryDao;

    @InjectMocks
    private VerifiedGrossMassService verifiedGrossMassService;
    @Mock
    private ResponseHelper responseHelper;

    @Mock
    private IContainerDao containerDao;

    private VerifiedGrossMassRequest testRequest;
    private VerifiedGrossMass testEntity;
    private VerifiedGrossMassResponse testResponse;
    private CarrierBooking testCarrierBooking;
    private ConsolidationDetails testConsolidationDetails;
    private SubmitAmendInttraRequest request;

    @BeforeEach
    void setUp() {
        testRequest = createTestRequest();
        testEntity = createTestEntity();
        testResponse = createTestResponse();
        testCarrierBooking = createTestCarrierBooking();
        testConsolidationDetails = createTestConsolidationDetails();
    }

    @Test
    void testUpdate_whenMatchingId_shouldCopyContainerRefGuid() {
        UUID refGuid = UUID.randomUUID();

        CommonContainers existing = new CommonContainers();
        existing.setId(1L);
        existing.setContainerRefGuid(refGuid);

        VerifiedGrossMass existingEntity = new VerifiedGrossMass();
        existingEntity.setEntityId(100L);
        existingEntity.setSailingInformation(new SailingInformation());
        existingEntity.setContainersList(List.of(existing));

        CommonContainers incoming = new CommonContainers();
        incoming.setId(1L);

        VerifiedGrossMass mapped = new VerifiedGrossMass();
        mapped.setSailingInformation(new SailingInformation());
        mapped.setContainersList(List.of(incoming));

        VerifiedGrossMass saved = new VerifiedGrossMass();

        VerifiedGrossMassRequest request = new VerifiedGrossMassRequest();
        request.setId(1L);
        request.setEntityId(100L);
        request.setEntityType(EntityType.CONSOLIDATION);

        when(verifiedGrossMassDao.findById(1L)).thenReturn(Optional.of(existingEntity));
        when(jsonHelper.convertValue(request, VerifiedGrossMass.class)).thenReturn(mapped);
        when(verifiedGrossMassValidationUtil.validateRequest(any(), any())).thenReturn(new ConsolidationDetails());
        when(verifiedGrossMassDao.save(mapped)).thenReturn(saved);
        when(jsonHelper.convertValue(saved, VerifiedGrossMassResponse.class)).thenReturn(new VerifiedGrossMassResponse());

        verifiedGrossMassService.update(request);

        assertEquals(refGuid, incoming.getContainerRefGuid());
    }

    @Test
    void testUpdate_whenIncomingIdIsNull_shouldSkipMapping() {
        UUID refGuid = UUID.randomUUID();

        CommonContainers existing = new CommonContainers();
        existing.setId(1L);
        existing.setContainerRefGuid(refGuid);

        VerifiedGrossMass existingEntity = new VerifiedGrossMass();
        existingEntity.setEntityId(100L);
        existingEntity.setSailingInformation(new SailingInformation());
        existingEntity.setContainersList(List.of(existing));

        CommonContainers incoming = new CommonContainers();
        incoming.setId(null); // incoming ID is null

        VerifiedGrossMass mapped = new VerifiedGrossMass();
        mapped.setSailingInformation(new SailingInformation());
        mapped.setContainersList(List.of(incoming));

        VerifiedGrossMass saved = new VerifiedGrossMass();

        VerifiedGrossMassRequest request = new VerifiedGrossMassRequest();
        request.setId(1L);
        request.setEntityId(100L);
        request.setEntityType(EntityType.CONSOLIDATION);

        when(verifiedGrossMassDao.findById(1L)).thenReturn(Optional.of(existingEntity));
        when(jsonHelper.convertValue(request, VerifiedGrossMass.class)).thenReturn(mapped);
        when(verifiedGrossMassValidationUtil.validateRequest(any(), any())).thenReturn(new ConsolidationDetails());
        when(verifiedGrossMassDao.save(mapped)).thenReturn(saved);
        when(jsonHelper.convertValue(saved, VerifiedGrossMassResponse.class)).thenReturn(new VerifiedGrossMassResponse());

        verifiedGrossMassService.update(request);

        assertNull(incoming.getContainerRefGuid());
    }

    @Test
    void testUpdate_whenIncomingIdDoesNotMatchExisting_shouldSkipMapping() {
        UUID refGuid = UUID.randomUUID();

        CommonContainers existing = new CommonContainers();
        existing.setId(1L);
        existing.setContainerRefGuid(refGuid);

        VerifiedGrossMass existingEntity = new VerifiedGrossMass();
        existingEntity.setEntityId(100L);
        existingEntity.setSailingInformation(new SailingInformation());
        existingEntity.setContainersList(List.of(existing));

        CommonContainers incoming = new CommonContainers();
        incoming.setId(999L); // ID not matching

        VerifiedGrossMass mapped = new VerifiedGrossMass();
        mapped.setSailingInformation(new SailingInformation());
        mapped.setContainersList(List.of(incoming));

        VerifiedGrossMass saved = new VerifiedGrossMass();

        VerifiedGrossMassRequest request = new VerifiedGrossMassRequest();
        request.setId(1L);
        request.setEntityId(100L);
        request.setEntityType(EntityType.CONSOLIDATION);

        when(verifiedGrossMassDao.findById(1L)).thenReturn(Optional.of(existingEntity));
        when(jsonHelper.convertValue(request, VerifiedGrossMass.class)).thenReturn(mapped);
        when(verifiedGrossMassValidationUtil.validateRequest(any(), any())).thenReturn(new ConsolidationDetails());
        when(verifiedGrossMassDao.save(mapped)).thenReturn(saved);
        when(jsonHelper.convertValue(saved, VerifiedGrossMassResponse.class)).thenReturn(new VerifiedGrossMassResponse());

        verifiedGrossMassService.update(request);

        assertNull(incoming.getContainerRefGuid());
    }

    @Test
    void testUpdate_whenExistingContainerListIsNull_shouldNotCrash() {
        VerifiedGrossMass existingEntity = new VerifiedGrossMass();
        existingEntity.setEntityId(100L);
        existingEntity.setSailingInformation(new SailingInformation());
        existingEntity.setContainersList(null); // Null containers

        CommonContainers incoming = new CommonContainers();
        incoming.setId(1L);

        VerifiedGrossMass mapped = new VerifiedGrossMass();
        mapped.setSailingInformation(new SailingInformation());
        mapped.setContainersList(List.of(incoming));

        VerifiedGrossMass saved = new VerifiedGrossMass();

        VerifiedGrossMassRequest request = new VerifiedGrossMassRequest();
        request.setId(1L);
        request.setEntityId(100L);
        request.setEntityType(EntityType.CONSOLIDATION);

        when(verifiedGrossMassDao.findById(1L)).thenReturn(Optional.of(existingEntity));
        when(jsonHelper.convertValue(request, VerifiedGrossMass.class)).thenReturn(mapped);
        when(verifiedGrossMassValidationUtil.validateRequest(any(), any())).thenReturn(new ConsolidationDetails());
        when(verifiedGrossMassDao.save(mapped)).thenReturn(saved);
        when(jsonHelper.convertValue(saved, VerifiedGrossMassResponse.class)).thenReturn(new VerifiedGrossMassResponse());

        verifiedGrossMassService.update(request);

        assertNull(incoming.getContainerRefGuid());
    }

    @Test
    void syncContainersByIds_ShouldSyncMatchingContainers() {
        UUID refGuid = UUID.randomUUID();
        Long containerId = 1L;

        CommonContainers common = new CommonContainers();
        common.setId(containerId);
        common.setContainerRefGuid(refGuid);

        List<CommonContainers> commonContainersList = List.of(common);

        Containers container = new Containers();
        container.setGuid(refGuid);
        container.setContainerCode("CODE123");
        container.setContainerNumber("NUM456");
        container.setNetWeight(new BigDecimal("100.5"));
        container.setTareWeight(new BigDecimal("10.0"));
        container.setTareWeightUnit("KG");
        container.setGrossWeight(new BigDecimal("110.5"));
        container.setGrossWeightUnit("KG");
        container.setCarrierSealNumber("CS123");
        container.setShipperSealNumber("SS456");
        container.setCustomsSealNumber("CUS789");
        container.setVeterinarySealNumber("VET000");

        when(commonContainersRepository.findAllById(List.of(containerId))).thenReturn(commonContainersList);
        when(containerDao.findAllByGuid(List.of(refGuid))).thenReturn(List.of(container));
        when(commonContainersRepository.saveAll(commonContainersList)).thenReturn(commonContainersList);

        CommonContainerResponse response = new CommonContainerResponse();
        response.setId(containerId);
        when(jsonHelper.convertValue(common, CommonContainerResponse.class)).thenReturn(response);

        List<CommonContainerResponse> result = verifiedGrossMassService.syncContainersByIds(List.of(containerId));

        assertEquals(1, result.size());
        assertEquals(containerId, result.get(0).getId());

        // Validate data is updated from container
        assertEquals("CODE123", common.getContainerCode());
        assertEquals("NUM456", common.getContainerNo());
        assertEquals(new BigDecimal("100.5"), common.getNetWeight());
        assertEquals(new BigDecimal("110.5"), common.getGrossWeight());
        assertEquals("KG", common.getGrossWeightUnit());
        assertEquals("CS123", common.getSealNumber());
    }

    @Test
    void syncContainersByIds_ShouldSkipWhenRefGuidIsNull() {
        CommonContainers common = new CommonContainers();
        common.setId(1L);
        common.setContainerRefGuid(null); // null guid

        List<CommonContainers> list = List.of(common);

        when(commonContainersRepository.findAllById(List.of(1L))).thenReturn(list);
        when(containerDao.findAllByGuid(List.of())).thenReturn(List.of());
        when(commonContainersRepository.saveAll(list)).thenReturn(list);

        CommonContainerResponse response = new CommonContainerResponse();
        response.setId(1L);
        when(jsonHelper.convertValue(common, CommonContainerResponse.class)).thenReturn(response);

        List<CommonContainerResponse> result = verifiedGrossMassService.syncContainersByIds(List.of(1L));

        assertEquals(1, result.size());
        verify(containerDao).findAllByGuid(List.of());
    }

    @Test
    void syncContainersByIds_ShouldSkipWhenContainerNotFound() {
        UUID refGuid = UUID.randomUUID();
        CommonContainers common = new CommonContainers();
        common.setId(1L);
        common.setContainerRefGuid(refGuid);

        List<CommonContainers> list = List.of(common);

        when(commonContainersRepository.findAllById(List.of(1L))).thenReturn(list);
        when(containerDao.findAllByGuid(List.of(refGuid))).thenReturn(List.of()); // no matching containers
        when(commonContainersRepository.saveAll(list)).thenReturn(list);

        CommonContainerResponse response = new CommonContainerResponse();
        response.setId(1L);
        when(jsonHelper.convertValue(common, CommonContainerResponse.class)).thenReturn(response);

        List<CommonContainerResponse> result = verifiedGrossMassService.syncContainersByIds(List.of(1L));

        assertEquals(1, result.size());
        // Check that no update happened
        assertNull(common.getContainerCode());
        assertNull(common.getGrossWeight());
    }

    @Test
    void bulkUpdateContainers_ShouldThrowValidationException_WhenLessThanTwoIds() {
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(List.of(1L)); // only 1 id
        assertThrows(ValidationException.class, () ->
                verifiedGrossMassService.bulkUpdateContainers(request)
        );
    }

    @Test
    void bulkUpdateContainers_ShouldUpdateSuccessfully() {
        // Arrange
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(Arrays.asList(1L, 2L));
        request.setApprovalSignature("abc");
        // create weighingParty
        Parties party = new Parties();
        party.setId(1001L);
        party.setOrgCode("ORG123");
        party.setType("SHIPPER");
        request.setWeighingParty(party);

        CommonContainers c1 = new CommonContainers();
        c1.setId(1L);
        c1.setApprovalSignature("abc");
        c1.setWeighingParty(party);

        CommonContainers c2 = new CommonContainers();
        c2.setId(2L);
        c2.setApprovalSignature("abc");
        c2.setWeighingParty(party);
        List<CommonContainers> containers = Arrays.asList(c1, c2);

        when(commonContainersRepository.findAllByIdIn(request.getContainerIds()))
                .thenReturn(containers);
        when(commonContainersRepository.saveAll(containers))
                .thenReturn(containers);

        PartiesResponse partyResponse = PartiesResponse.builder()
                .id(party.getId())
                .orgCode(party.getOrgCode())
                .type(party.getType())
                .build();


        // Mock JsonHelper conversion
        when(jsonHelper.convertValue(any(CommonContainers.class), eq(CommonContainerResponse.class)))
                .thenAnswer(invocation -> {
                    CommonContainers c = invocation.getArgument(0);
                    CommonContainerResponse r = new CommonContainerResponse();
                    r.setId(c.getId());
                    r.setApprovalSignature(c.getApprovalSignature());
                    r.setWeighingParty(partyResponse);
                    return r;
                });

        // Act
        List<CommonContainerResponse> result = verifiedGrossMassService.bulkUpdateContainers(request);

        // Assert
        assertEquals(2, result.size());
        assertEquals("ABC", result.get(0).getApprovalSignature()); // signature should be uppercase
        assertEquals("ORG123", result.get(0).getWeighingParty().getOrgCode());
    }


    @Test
    void bulkUpdateContainers_ShouldThrowValidationException_WhenContainerNotFound() {
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(Arrays.asList(1L, 2L));

        when(commonContainersRepository.findAllByIdIn(request.getContainerIds()))
                .thenReturn(Collections.singletonList(new CommonContainers())); // only 1 returned

        assertThrows(ValidationException.class,
                () -> verifiedGrossMassService.bulkUpdateContainers(request));
    }

    @Test
    void bulkUpdateContainers_ShouldUpdateApprovalDate() {
        List<Long> ids = Arrays.asList(1L, 2L);
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(ids);
        request.setApprovalDate(LocalDateTime.of(2025, 1, 1, 12, 0));

        CommonContainers c1 = new CommonContainers();
        CommonContainers c2 = new CommonContainers();
        List<CommonContainers> containers = Arrays.asList(c1, c2);

        when(commonContainersRepository.findAllByIdIn(ids)).thenReturn(containers);
        when(commonContainersRepository.saveAll(anyList())).thenReturn(containers);

        when(jsonHelper.convertValue(any(CommonContainers.class), eq(CommonContainerResponse.class)))
                .thenReturn(new CommonContainerResponse());

        List<CommonContainerResponse> responses = verifiedGrossMassService.bulkUpdateContainers(request);

        assertEquals(LocalDateTime.of(2025, 1, 1, 12, 0), containers.get(0).getApprovalDate());
        assertEquals(LocalDateTime.of(2025, 1, 1, 12, 0), containers.get(1).getApprovalDate());
        assertEquals(2, responses.size());
    }

    @Test
    void bulkUpdateContainers_ShouldUpdateOnlyProvidedFields() {
        // Arrange
        List<Long> ids = Arrays.asList(1L, 2L);
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(ids);
        request.setApprovalSignature("john"); // only updating signature

        CommonContainers c1 = new CommonContainers();
        CommonContainers c2 = new CommonContainers();
        List<CommonContainers> containers = Arrays.asList(c1, c2);

        when(commonContainersRepository.findAllByIdIn(ids)).thenReturn(containers);
        when(commonContainersRepository.saveAll(anyList())).thenReturn(containers);

        when(jsonHelper.convertValue(any(CommonContainers.class), eq(CommonContainerResponse.class)))
                .thenReturn(new CommonContainerResponse());

        // Act
        List<CommonContainerResponse> responses = verifiedGrossMassService.bulkUpdateContainers(request);

        // Assert
        assertEquals("JOHN", containers.get(0).getApprovalSignature()); // uppercase applied
        assertEquals("JOHN", containers.get(1).getApprovalSignature());
        assertEquals(2, responses.size());
    }

    @Test
    void bulkUpdateContainers_ShouldUpdateWeightDeterminationFields() {
        // Arrange
        List<Long> ids = Arrays.asList(1L, 2L);
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(ids);
        request.setWeightDeterminationMethod(WeightDeterminationMethodType.METHOD1);
        request.setWeightDeterminationLocation("Port of LA");

        CommonContainers c1 = new CommonContainers();
        CommonContainers c2 = new CommonContainers();
        List<CommonContainers> containers = Arrays.asList(c1, c2);

        when(commonContainersRepository.findAllByIdIn(ids)).thenReturn(containers);
        when(commonContainersRepository.saveAll(anyList())).thenReturn(containers);

        when(jsonHelper.convertValue(any(CommonContainers.class), eq(CommonContainerResponse.class)))
                .thenReturn(new CommonContainerResponse());

        // Act
        List<CommonContainerResponse> responses = verifiedGrossMassService.bulkUpdateContainers(request);

        // Assert
        assertEquals(WeightDeterminationMethodType.METHOD1, c1.getWeightDeterminationMethod());
        assertEquals("Port of LA", c1.getWeightDeterminationLocation());
        assertEquals(WeightDeterminationMethodType.METHOD1, c2.getWeightDeterminationMethod());
        assertEquals("Port of LA", c2.getWeightDeterminationLocation());
        assertEquals(2, responses.size());

        // Verify saveAll called once
        verify(commonContainersRepository, times(1)).saveAll(containers);
    }

    @Test
    void testCreate_WithCarrierBooking_Success() {
        // Arrange
        when(verifiedGrossMassValidationUtil.validateRequest(EntityType.CARRIER_BOOKING, 1L))
                .thenReturn(testCarrierBooking);
        when(jsonHelper.convertValue(testRequest, VerifiedGrossMass.class))
                .thenReturn(testEntity);
        when(verifiedGrossMassDao.save(any(VerifiedGrossMass.class)))
                .thenReturn(testEntity);
        when(jsonHelper.convertValue(testEntity, VerifiedGrossMassResponse.class))
                .thenReturn(testResponse);

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            UsersDto mockUser = mock(UsersDto.class);
            when(mockUser.getEmail()).thenReturn("carrieruser@example.com");
            userContext.when(UserContext::getUser).thenReturn(mockUser);

            // Act
            VerifiedGrossMassResponse result = verifiedGrossMassService.create(testRequest);

            // Assert
            assertNotNull(result);
            verify(verifiedGrossMassValidationUtil).validateServiceType(testRequest);
            verify(verifiedGrossMassValidationUtil).validateRequest(EntityType.CARRIER_BOOKING, 1L);
            verify(verifiedGrossMassDao).save(any(VerifiedGrossMass.class));
            assertEquals(VerifiedGrossMassStatus.Draft, testEntity.getStatus());
        }
    }

    @Test
    void testCreate_WithConsolidation_Success() {
        // Arrange
        testRequest.setEntityType(EntityType.CONSOLIDATION);

        when(verifiedGrossMassValidationUtil.validateRequest(EntityType.CONSOLIDATION, 1L))
                .thenReturn(testConsolidationDetails);
        when(jsonHelper.convertValue(testRequest, VerifiedGrossMass.class))
                .thenReturn(testEntity);
        when(verifiedGrossMassDao.save(any(VerifiedGrossMass.class)))
                .thenReturn(testEntity);
        when(jsonHelper.convertValue(testEntity, VerifiedGrossMassResponse.class))
                .thenReturn(testResponse);

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            UsersDto mockUser = Mockito.mock(UsersDto.class);
            when(mockUser.getEmail()).thenReturn("testuser@example.com");
            userContext.when(UserContext::getUser).thenReturn(mockUser);

            VerifiedGrossMassResponse result = verifiedGrossMassService.create(testRequest);

            assertNotNull(result);
            verify(verifiedGrossMassValidationUtil).validateServiceType(testRequest);
            verify(verifiedGrossMassDao).save(any(VerifiedGrossMass.class));
        }
    }

    @Test
    void testRetrieveById_Success() {
        CarrierBookingInfoProjection projection = new CarrierBookingInfoProjection() {
            @Override
            public String getBookingStatus() {
                return "Draft";
            }

            @Override
            public String getBookingNo() {
                return "BOOK123";
            }

            @Override
            public String getSiStatus() {
                return "Draft";
            }
        };
        when(verifiedGrossMassDao.findById(1L))
                .thenReturn(Optional.of(testEntity));
        when(jsonHelper.convertValue(testEntity, VerifiedGrossMassResponse.class))
                .thenReturn(testResponse);
        when(carrierBookingDao.findCarrierBookingInfoById(anyLong())).thenReturn(projection);
        // Act
        VerifiedGrossMassResponse result = verifiedGrossMassService.retrieveById(1L);

        // Assert [web:3]
        assertNotNull(result);
        verify(verifiedGrossMassDao).findById(1L);
    }

    @Test
    void testRetrieveById_WithCarrierBooking_Success() {
        // Arrange [web:7]
        testEntity.setEntityType(EntityType.CARRIER_BOOKING);
        CarrierBookingInfoProjection projection = mock(CarrierBookingInfoProjection.class);
        when(projection.getBookingStatus()).thenReturn("Draft");
        when(projection.getSiStatus()).thenReturn("Draft");

        when(verifiedGrossMassDao.findById(1L))
                .thenReturn(Optional.of(testEntity));
        when(jsonHelper.convertValue(testEntity, VerifiedGrossMassResponse.class))
                .thenReturn(testResponse);
        when(carrierBookingDao.findCarrierBookingInfoById(1L))
                .thenReturn(projection);

        // Act
        VerifiedGrossMassResponse result = verifiedGrossMassService.retrieveById(1L);

        // Assert [web:7]
        assertNotNull(result);
        verify(carrierBookingDao).findCarrierBookingInfoById(1L);
        assertEquals(CarrierBookingStatus.Draft, result.getBookingStatus());
        assertEquals(ShippingInstructionStatus.Draft, result.getSiStatus());
    }

    @Test
    void testRetrieveById_NotFound_ThrowsException() {
        // Arrange [web:3]
        when(verifiedGrossMassDao.findById(1L))
                .thenReturn(Optional.empty());

        // Act & Assert [web:3]
        ValidationException exception = assertThrows(ValidationException.class,
                () -> verifiedGrossMassService.retrieveById(1L));

        assertEquals("Invalid vgm id", exception.getMessage());
    }

    @Test
    void testList_Success() {
        // Arrange [web:7]
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ListCommonRequest listRequest = new ListCommonRequest();
        listRequest.setIncludeColumns(Arrays.asList("id", "entityNumber"));
        commonRequestModel.setData(listRequest);

        Page<VerifiedGrossMass> page = new PageImpl<>(Arrays.asList(testEntity));

        when(verifiedGrossMassDao.findAll(any(Specification.class), any(Pageable.class)))
                .thenReturn(page);

        // Act
        ResponseEntity<IRunnerResponse> result = verifiedGrossMassService.list(commonRequestModel, false);

        // Assert [web:7]
        assertNotNull(result);
        assertTrue(result.getStatusCode().is2xxSuccessful());
        verify(verifiedGrossMassDao).findAll(any(Specification.class), any(Pageable.class));
    }

    @Test
    void testList_EmptyRequest_ThrowsException() {
        // Arrange [web:3]
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();

        // Act & Assert [web:3]
        ValidationException exception = assertThrows(ValidationException.class,
                () -> verifiedGrossMassService.list(commonRequestModel, false));

        assertEquals(VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_LIST_REQUEST_NULL_ERROR,
                exception.getMessage());
    }

    @Test
    void testUpdate_Success() {
        // Arrange [web:3]
        testRequest.setId(1L);
        when(verifiedGrossMassDao.findById(1L))
                .thenReturn(Optional.of(testEntity));
        when(verifiedGrossMassValidationUtil.validateRequest(EntityType.CARRIER_BOOKING, 1L))
                .thenReturn(testCarrierBooking);
        when(jsonHelper.convertValue(testRequest, VerifiedGrossMass.class))
                .thenReturn(testEntity);
        when(verifiedGrossMassDao.save(any(VerifiedGrossMass.class)))
                .thenReturn(testEntity);
        when(jsonHelper.convertValue(testEntity, VerifiedGrossMassResponse.class))
                .thenReturn(testResponse);

        // Act
        VerifiedGrossMassResponse result = verifiedGrossMassService.update(testRequest);

        // Assert [web:3]
        assertNotNull(result);
        verify(verifiedGrossMassDao).save(any(VerifiedGrossMass.class));
    }

    @Test
    void testUpdate_NullId_ThrowsException() {
        // Arrange [web:7]
        testRequest.setId(null);

        // Act & Assert [web:7]
        ValidationException exception = assertThrows(ValidationException.class,
                () -> verifiedGrossMassService.update(testRequest));

        assertEquals("Id can not be null", exception.getMessage());
    }

    @Test
    void testUpdate_InvalidId_ThrowsException() {
        // Arrange [web:3]
        testRequest.setId(1L);
        when(verifiedGrossMassDao.findById(1L))
                .thenReturn(Optional.empty());

        // Act & Assert [web:3]
        ValidationException exception = assertThrows(ValidationException.class,
                () -> verifiedGrossMassService.update(testRequest));

        assertEquals("Invalid verified gross mass id", exception.getMessage());
    }

    @Test
    void testUpdate_EntityIdMismatch_ThrowsException() {
        // Arrange [web:7]
        testRequest.setId(1L);
        testRequest.setEntityId(2L); // Different from testEntity's entityId
        when(verifiedGrossMassDao.findById(1L))
                .thenReturn(Optional.of(testEntity));

        // Act & Assert [web:7]
        ValidationException exception = assertThrows(ValidationException.class,
                () -> verifiedGrossMassService.update(testRequest));

        assertEquals("Entity Id mismatch with existing entity id", exception.getMessage());
    }

    @Test
    void testDelete_Success() {
        // Arrange & Act [web:3]
        verifiedGrossMassService.delete(1L);

        // Assert [web:3]
        verify(verifiedGrossMassDao).delete(1L);
    }

    @Test
    void testGetAllMasterData_whenVgmNotFound_shouldThrow() {
        when(verifiedGrossMassDao.findById(1L)).thenReturn(Optional.empty());
        ResponseEntity<IRunnerResponse> responseEntity = verifiedGrossMassService.getAllMasterData(1L);
        assertEquals(responseEntity.getStatusCodeValue(), HttpStatus.BAD_REQUEST.value());
    }

    @Test
    void testGetAllMasterData_whenVgmExists_shouldReturnMasterDataMap() {
        // Given
        Long vgmId = 123L;

        VerifiedGrossMassResponse verifiedGrossMassResponse = new VerifiedGrossMassResponse();
        Map<String, Object> dummyMasterData = Map.of("key1", "value1");

        when(verifiedGrossMassDao.findById(vgmId)).thenReturn(Optional.of(testEntity));

        when(commonUtils.setIncludedFieldsToResponse(any(), anySet(), any()))
                .thenReturn(verifiedGrossMassResponse);

        VerifiedGrossMassService spyService = Mockito.spy(verifiedGrossMassService);
        doReturn(dummyMasterData).when(spyService).fetchAllMasterDataByKey(verifiedGrossMassResponse);

        ResponseEntity<IRunnerResponse> responseEntity = spyService.getAllMasterData(vgmId);

        assertNotNull(responseEntity);

        verify(verifiedGrossMassDao).findById(vgmId);
        verify(commonUtils).setIncludedFieldsToResponse(eq(testEntity), anySet(), any(VerifiedGrossMassResponse.class));
        verify(spyService).fetchAllMasterDataByKey(verifiedGrossMassResponse);
    }

    @Test
    void testGetAllMasterData_NotFound_ReturnsFailure() {
        // Arrange [web:3]
        when(verifiedGrossMassDao.findById(1L))
                .thenReturn(Optional.empty());

        // Act
        ResponseEntity<IRunnerResponse> result = verifiedGrossMassService.getAllMasterData(1L);

        // Assert [web:3]
        assertNotNull(result);
        assertFalse(result.getStatusCode().is2xxSuccessful());
    }

    @Test
    void testGetDefaultVerifiedGrossMassValues_CarrierBooking_Success() {
        // Arrange [web:7]
        when(carrierBookingDao.findById(1L))
                .thenReturn(Optional.of(testCarrierBooking));
        when(jsonHelper.convertValue(any(), eq(PartiesResponse.class)))
                .thenReturn(new PartiesResponse());
        when(jsonHelper.convertValueToList(any(), eq(CommonContainerResponse.class)))
                .thenReturn(Arrays.asList(new CommonContainerResponse()));

        // Act
        VerifiedGrossMassResponse result = verifiedGrossMassService
                .getDefaultVerifiedGrossMassValues(EntityType.CARRIER_BOOKING, 1L);

        // Assert [web:7]
        assertNotNull(result);
        assertEquals(EntityType.CARRIER_BOOKING, result.getEntityType());
        assertEquals(1L, result.getEntityId());
        verify(carrierBookingDao).findById(1L);
    }

    @Test
    void testGetDefaultVerifiedGrossMassValues_Consolidation_Success() {
        // Arrange [web:10]
        when(consolidationDetailsDao.findConsolidationsById(1L))
                .thenReturn(testConsolidationDetails);
        when(jsonHelper.convertValue(any(), eq(PartiesResponse.class)))
                .thenReturn(new PartiesResponse());

        // Act
        VerifiedGrossMassResponse result = verifiedGrossMassService
                .getDefaultVerifiedGrossMassValues(EntityType.CONSOLIDATION, 1L);

        // Assert [web:10]
        assertNotNull(result);
        assertEquals(EntityType.CONSOLIDATION, result.getEntityType());
        assertEquals(1L, result.getEntityId());
        verify(consolidationDetailsDao).findConsolidationsById(1L);
    }

    @Test
    void testGetDefaultVerifiedGrossMassValues_InvalidEntityType_ThrowsException() {
        // Act & Assert [web:3]
        ValidationException exception = assertThrows(ValidationException.class,
                () -> verifiedGrossMassService.getDefaultVerifiedGrossMassValues(null, 1L));

        assertEquals("Invalid value of Entity Type", exception.getMessage());
    }

    @Test
    void testGetDefaultVerifiedGrossMassValues_InvalidCarrierBookingId_ThrowsException() {
        // Arrange [web:7]
        when(carrierBookingDao.findById(1L))
                .thenReturn(Optional.empty());

        // Act & Assert [web:7]
        ValidationException exception = assertThrows(ValidationException.class,
                () -> verifiedGrossMassService.getDefaultVerifiedGrossMassValues(EntityType.CARRIER_BOOKING, 1L));

        assertEquals("Invalid carrier booking id", exception.getMessage());
    }

    @Test
    void testGetDefaultVerifiedGrossMassValues_InvalidConsolidationId_ThrowsException() {
        // Arrange [web:3]
        when(consolidationDetailsDao.findConsolidationsById(1L))
                .thenReturn(null);

        // Act & Assert [web:3]
        ValidationException exception = assertThrows(ValidationException.class,
                () -> verifiedGrossMassService.getDefaultVerifiedGrossMassValues(EntityType.CONSOLIDATION, 1L));

        assertEquals("Invalid consolidation id", exception.getMessage());
    }

    @Test
    void testBulkUpdateContainers_Success() {
        // Arrange [web:7]
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(Arrays.asList(1L, 2L));
        request.setWeightDeterminationMethod(WeightDeterminationMethodType.METHOD1);
        request.setWeightDeterminationLocation("Location");
        request.setApprovalSignature("signature");
        request.setApprovalDate(LocalDateTime.now());

        List<CommonContainers> containers = Arrays.asList(
                createTestCommonContainer(1L),
                createTestCommonContainer(2L)
        );

        when(commonContainersRepository.findAllByIdIn(request.getContainerIds()))
                .thenReturn(containers);
        when(commonContainersRepository.saveAll(containers))
                .thenReturn(containers);
        when(jsonHelper.convertValue(any(CommonContainers.class), eq(CommonContainerResponse.class)))
                .thenReturn(new CommonContainerResponse());

        // Act
        List<CommonContainerResponse> result = verifiedGrossMassService.bulkUpdateContainers(request);

        // Assert [web:7]
        assertNotNull(result);
        assertEquals(2, result.size());
        verify(commonContainersRepository).saveAll(containers);

        // Verify that containers were updated
        for (CommonContainers container : containers) {
            assertEquals(WeightDeterminationMethodType.METHOD1, container.getWeightDeterminationMethod());
            assertEquals("Location", container.getWeightDeterminationLocation());
            assertEquals("SIGNATURE", container.getApprovalSignature()); // Should be uppercase
        }
    }

    @Test
    void testBulkUpdateContainers_ContainersNotFound_ThrowsException() {
        // Arrange [web:3]
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(Arrays.asList(1L, 2L));

        when(commonContainersRepository.findAllByIdIn(request.getContainerIds()))
                .thenReturn(Arrays.asList(createTestCommonContainer(1L))); // Only 1 container found

        // Act & Assert [web:3]
        ValidationException exception = assertThrows(ValidationException.class,
                () -> verifiedGrossMassService.bulkUpdateContainers(request));

        assertEquals("Some containers could not be found", exception.getMessage());
    }

    @Test
    void testBulkUpdateContainers_NullContainers_ThrowsException() {
        // Arrange [web:7]
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(Arrays.asList(1L, 2L));

        when(commonContainersRepository.findAllByIdIn(request.getContainerIds()))
                .thenReturn(null);

        // Act & Assert [web:7]
        ValidationException exception = assertThrows(ValidationException.class,
                () -> verifiedGrossMassService.bulkUpdateContainers(request));

        assertEquals("Some containers could not be found", exception.getMessage());
    }

    // Helper methods to create test data [web:10]
    private VerifiedGrossMassRequest createTestRequest() {
        VerifiedGrossMassRequest request = new VerifiedGrossMassRequest();
        request.setEntityType(EntityType.CARRIER_BOOKING);
        request.setEntityId(1L);
        return request;
    }

    private VerifiedGrossMass createTestEntity() {
        VerifiedGrossMass entity = new VerifiedGrossMass();
        entity.setId(1L);
        entity.setEntityType(EntityType.CARRIER_BOOKING);
        entity.setEntityId(1L);
        entity.setStatus(VerifiedGrossMassStatus.Draft);
        entity.setSailingInformation(new SailingInformation());
        return entity;
    }

    private VerifiedGrossMassResponse createTestResponse() {
        VerifiedGrossMassResponse response = new VerifiedGrossMassResponse();
        response.setId(1L);
        response.setEntityType(EntityType.CARRIER_BOOKING);
        response.setEntityId(1L);
        return response;
    }

    private CarrierBooking createTestCarrierBooking() {
        CarrierBooking booking = new CarrierBooking();
        booking.setId(1L);
        booking.setBookingNo("BOOK123");
        booking.setCarrierBookingNo("CB123");
        booking.setCarrierBlNo("BL123");
        booking.setStatus(CarrierBookingStatus.Draft);
        booking.setSailingInformation(new SailingInformation());
        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        referenceNumbers.setType("SRN");
        referenceNumbers.setReferenceNumber("srn123");
        List<ReferenceNumbers> referenceNumbersList = new ArrayList<>();
        referenceNumbersList.add(referenceNumbers);
        booking.setReferenceNumbersList(referenceNumbersList);

        Parties party = new Parties();
        party.setId(1001L);
        party.setOrgCode("ORG123");
        party.setType("SHIPPER");

        CommonContainers c1 = new CommonContainers();
        c1.setId(1L);
        CommonContainers c2 = new CommonContainers();
        c2.setId(2L);
        List<CommonContainers> containers = Arrays.asList(c1, c2);

        booking.setContainersList(containers);
        booking.setRequester(party);
        booking.setShipper(party);
        booking.setForwardingAgent(party);

        return booking;
    }

    private ConsolidationDetails createTestConsolidationDetails() {
        ConsolidationDetails details = new ConsolidationDetails();
        details.setId(1L);
        details.setConsolidationNumber("CONSOL123");
        details.setCarrierDetails(new CarrierDetails());
        details.getCarrierDetails().setShippingLine("MSC");

        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        referenceNumbers.setType("SRN");
        referenceNumbers.setReferenceNumber("srn123");
        List<ReferenceNumbers> referenceNumbersList = new ArrayList<>();
        referenceNumbersList.add(referenceNumbers);
        details.setReferenceNumbersList(referenceNumbersList);

        Parties party = new Parties();
        party.setId(1001L);
        party.setOrgCode("ORG123");
        party.setType("SHIPPER");

        Containers c1 = new Containers();
        c1.setId(1L);
        Containers c2 = new Containers();
        c2.setId(2L);
        List<Containers> containers = Arrays.asList(c1, c2);
        details.setContainersList(containers);

        details.setSendingAgent(party);
        return details;
    }

    private CommonContainers createTestCommonContainer(Long id) {
        CommonContainers container = new CommonContainers();
        container.setId(id);
        container.setContainerNo("CONT" + id);
        return container;
    }

    @Test
    void testSubmitOrAmendVerifiedGrossMass_VgmNotFound_ShouldThrowException() {
        // Arrange
        SubmitAmendInttraRequest request = new SubmitAmendInttraRequest();
        request.setId(99L);
        when(verifiedGrossMassDao.findById(99L)).thenReturn(Optional.empty());

        // Act & Assert
        ValidationException ex = assertThrows(ValidationException.class, () ->
                verifiedGrossMassService.submitOrAmendVerifiedGrossMass(request)
        );

        assertEquals("Invalid VGM Id: 99", ex.getMessage());
    }

    @Test
    void testSubmitOrAmendVerifiedGrossMass_MultipleContainers_Submit() throws RunnerException {
        SubmitAmendInttraRequest request = new SubmitAmendInttraRequest();
        request.setId(1L);
        request.setContainerIds(List.of(101L, 102L));
        request.setOperationType(OperationType.SUBMIT);

        VerifiedGrossMass vgm = createMockVGM();

        when(verifiedGrossMassDao.findById(1L)).thenReturn(Optional.of(vgm));

        CommonContainers container1 = new CommonContainers();
        container1.setId(101L);
        CommonContainers container2 = new CommonContainers();
        container2.setId(102L);
        when(commonContainersRepository.findAllByIdIn(request.getContainerIds())).thenReturn(List.of(container1, container2));

        when(verifiedGrossMassUtil.buildContainerResponse(container1)).thenReturn(new CommonContainerResponse());
        when(verifiedGrossMassUtil.populateRequestorEmails(any())).thenReturn("test@example.com");
        when(verifiedGrossMassUtil.buildSubmittedContainer(container1)).thenReturn(container1);

        when(verifiedGrossMassUtil.buildContainerResponse(container2)).thenReturn(new CommonContainerResponse());
        when(verifiedGrossMassUtil.populateRequestorEmails(any())).thenReturn("test@example.com");
        when(verifiedGrossMassUtil.buildSubmittedContainer(container2)).thenReturn(container2);

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            UsersDto user = Mockito.mock(UsersDto.class);
            when(user.getUsername()).thenReturn("testUser");
            userContext.when(UserContext::getUser).thenReturn(user);

            verifiedGrossMassService.submitOrAmendVerifiedGrossMass(request);

            verify(verifiedGrossMassDao).save(any()); // assert save was called with updated VGM
            verify(carrierBookingInttraUtil, times(2)).sendPayloadToBridge(any(), anyLong(), anyString(), anyString(), anyString(), any(), any());
        }
    }

    @Test
    void testSubmitOrAmendVerifiedGrossMass_MultipleContainers_Amend() throws RunnerException {
        SubmitAmendInttraRequest request = new SubmitAmendInttraRequest();
        request.setId(1L);
        request.setContainerIds(List.of(101L, 102L));
        request.setOperationType(OperationType.AMEND);

        VerifiedGrossMass vgm = createMockVGM();

        when(verifiedGrossMassDao.findById(1L)).thenReturn(Optional.of(vgm));

        CommonContainers container1 = new CommonContainers();
        container1.setId(101L);
        CommonContainers container2 = new CommonContainers();
        container2.setId(102L);
        when(commonContainersRepository.findAllByIdIn(request.getContainerIds())).thenReturn(List.of(container1, container2));

        when(verifiedGrossMassUtil.buildContainerResponse(container1)).thenReturn(new CommonContainerResponse());
        when(verifiedGrossMassUtil.populateRequestorEmails(any())).thenReturn("test@example.com");
        when(verifiedGrossMassUtil.buildSubmittedContainer(container1)).thenReturn(container1);

        when(verifiedGrossMassUtil.buildContainerResponse(container2)).thenReturn(new CommonContainerResponse());
        when(verifiedGrossMassUtil.populateRequestorEmails(any())).thenReturn("test@example.com");
        when(verifiedGrossMassUtil.buildSubmittedContainer(container2)).thenReturn(container2);

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            UsersDto user = Mockito.mock(UsersDto.class);
            when(user.getUsername()).thenReturn("testUser");
            userContext.when(UserContext::getUser).thenReturn(user);

            verifiedGrossMassService.submitOrAmendVerifiedGrossMass(request);

            verify(verifiedGrossMassDao).save(any()); // assert save was called with updated VGM
            verify(carrierBookingInttraUtil, times(2)).sendPayloadToBridge(any(), anyLong(), anyString(), anyString(), anyString(), any(), any());
        }
    }

    @Test
    void testSubmitOrAmendVerifiedGrossMass_WithNullCarrierDetails() throws RunnerException {
        SubmitAmendInttraRequest verifiedGrossMassInttraRequest = new SubmitAmendInttraRequest();
        verifiedGrossMassInttraRequest.setId(1L);
        verifiedGrossMassInttraRequest.setContainerIds(List.of(101L));
        verifiedGrossMassInttraRequest.setOperationType(OperationType.SUBMIT);

        VerifiedGrossMass vgm = createMockVGM();
        CommonContainers container = new CommonContainers();
        container.setId(101L);

        when(verifiedGrossMassDao.findById(1L)).thenReturn(Optional.of(vgm));
        when(commonContainersRepository.findAllByIdIn(List.of(101L))).thenReturn(List.of(container));


        when(carrierBookingInttraUtil.fetchRequiredParty(any(Parties.class))).thenReturn(new PartiesResponse());
        when(verifiedGrossMassUtil.buildContainerResponse(container)).thenReturn(new CommonContainerResponse());
        when(verifiedGrossMassUtil.populateRequestorEmails(any())).thenReturn("test@example.com");
        when(verifiedGrossMassUtil.buildSubmittedContainer(container)).thenReturn(container);

        when(verifiedGrossMassUtil.fetchCarrierDetailsForBridgePayload(vgm)).thenReturn(null); // trigger null branch

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            UsersDto user = Mockito.mock(UsersDto.class);
            when(user.getUsername()).thenReturn("testUser");
            userContext.when(UserContext::getUser).thenReturn(user);

            verifiedGrossMassService.submitOrAmendVerifiedGrossMass(verifiedGrossMassInttraRequest);
        }

        verify(verifiedGrossMassDao).save(any()); // assert save called
    }

    @Test
    void testSubmitOrAmendVerifiedGrossMass_EmptyContainerList() throws RunnerException {
        SubmitAmendInttraRequest verifiedGrossMassInttraRequest = new SubmitAmendInttraRequest();
        verifiedGrossMassInttraRequest.setId(1L);
        verifiedGrossMassInttraRequest.setContainerIds(Collections.emptyList());
        verifiedGrossMassInttraRequest.setOperationType(OperationType.SUBMIT);

        VerifiedGrossMass vgm = createMockVGM();

        when(verifiedGrossMassDao.findById(1L)).thenReturn(Optional.of(vgm));
        when(commonContainersRepository.findAllByIdIn(Collections.emptyList())).thenReturn(Collections.emptyList());

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            UsersDto user = Mockito.mock(UsersDto.class);
            when(user.getUsername()).thenReturn("testUser");
            userContext.when(UserContext::getUser).thenReturn(user);

            verifiedGrossMassService.submitOrAmendVerifiedGrossMass(verifiedGrossMassInttraRequest);
        }

        verify(verifiedGrossMassDao).save(any());
    }

    @Test
    void retrieveById_ShouldReturnResponse_WhenCarrierBookingExistsWithProjection() {
        testEntity.setEntityType(EntityType.CARRIER_BOOKING);
        testEntity.setEntityId(100L);

        CarrierBookingInfoProjection projection = new CarrierBookingInfoProjection() {
            public String getBookingStatus() {
                return "Draft";
            }

            public String getBookingNo() {
                return "BK001";
            }

            public String getSiStatus() {
                return "Draft";
            }
        };

        when(verifiedGrossMassDao.findById(1L)).thenReturn(Optional.of(testEntity));
        when(jsonHelper.convertValue(testEntity, VerifiedGrossMassResponse.class)).thenReturn(testResponse);
        when(carrierBookingDao.findCarrierBookingInfoById(100L)).thenReturn(projection);

        VerifiedGrossMassResponse response = verifiedGrossMassService.retrieveById(1L);

        assertNotNull(response);
        verify(verifiedGrossMassDao).findById(1L);
        verify(jsonHelper).convertValue(testEntity, VerifiedGrossMassResponse.class);
        verify(carrierBookingDao).findCarrierBookingInfoById(100L);
    }

    @Test
    void retrieveById_ShouldHandleCarrierBookingLinkedToConsolidation() {
        testEntity.setEntityType(EntityType.CARRIER_BOOKING);
        testEntity.setEntityId(100L);
        testEntity.setContainersList(List.of(new CommonContainers()));
        testEntity.setSubmittedContainersList(List.of(new CommonContainers()));

        CarrierBooking carrierBooking = new CarrierBooking();
        carrierBooking.setEntityType(EntityType.CONSOLIDATION.name());
        carrierBooking.setEntityId(200L);

        ConsolidationDetails details = new ConsolidationDetails();
        details.setContainersList(List.of(new Containers()));

        when(verifiedGrossMassDao.findById(1L)).thenReturn(Optional.of(testEntity));
        when(jsonHelper.convertValue(testEntity, VerifiedGrossMassResponse.class)).thenReturn(testResponse);
        when(carrierBookingDao.findCarrierBookingInfoById(100L)).thenReturn(null); // projection null
        when(carrierBookingDao.findById(100L)).thenReturn(Optional.of(carrierBooking));
        when(carrierBookingInttraUtil.getConsolidationDetail(200L)).thenReturn(details);
        when(verifiedGrossMassUtil.compareVGMContainers(any(), any(), any())).thenReturn(List.of());

        VerifiedGrossMassResponse response = verifiedGrossMassService.retrieveById(1L);

        assertNotNull(response);
        verify(verifiedGrossMassDao).findById(1L);
        verify(carrierBookingDao).findById(100L);
        verify(verifiedGrossMassUtil, times(2)).compareVGMContainers(any(), any(), any());
    }

    @Test
    void retrieveById_ShouldHandleConsolidationEntityType() {
        testEntity.setEntityType(EntityType.CONSOLIDATION);
        testEntity.setEntityId(300L);
        testEntity.setContainersList(List.of(new CommonContainers()));
        testEntity.setSubmittedContainersList(List.of(new CommonContainers()));

        ConsolidationDetails details = new ConsolidationDetails();
        details.setContainersList(List.of(new Containers()));

        when(verifiedGrossMassDao.findById(1L)).thenReturn(Optional.of(testEntity));
        when(jsonHelper.convertValue(testEntity, VerifiedGrossMassResponse.class)).thenReturn(testResponse);
        when(carrierBookingInttraUtil.getConsolidationDetail(300L)).thenReturn(details);
        when(verifiedGrossMassUtil.compareVGMContainers(any(), any(), any())).thenReturn(List.of());

        VerifiedGrossMassResponse response = verifiedGrossMassService.retrieveById(1L);

        assertNotNull(response);
        verify(carrierBookingInttraUtil).getConsolidationDetail(300L);
        verify(verifiedGrossMassUtil, times(2)).compareVGMContainers(any(), any(), any());
    }


    private VerifiedGrossMass createMockVGM() {
        VerifiedGrossMass vgm = new VerifiedGrossMass();
        vgm.setId(1L);
        vgm.setCarrierBookingNo("CB123");
        vgm.setStatus(VerifiedGrossMassStatus.Draft);
        vgm.setIsDelegated(true);

        Parties party = new Parties();
        party.setOrgCode("ORG");
        vgm.setRequestor(party);
        vgm.setResponsible(party);
        vgm.setAuthorised(party);

        return vgm;
    }

    @Test
    void testSendNotification_Success() throws JsonProcessingException {
        VerifiedGrossMass vgm = createMockVGM();  // Mock VGM object

        // Mock email template response
        EmailTemplatesRequest emailTemplate = new EmailTemplatesRequest();
        emailTemplate.setType(VERIFIED_GROSS_MASS_EMAIL_TEMPLATE);
        emailTemplate.setSubject("Test Subject");
        emailTemplate.setBody("Test Body");

        // Mocking fetchEmailTemplate to return the email template
        when(carrierBookingInttraUtil.fetchEmailTemplate(anyList())).thenReturn(List.of(emailTemplate));
        when(verifiedGrossMassUtil.getSendEmailBaseRequest(vgm)).thenReturn(new ArrayList<>());

        // Mock the email sending
        when(notificationService.sendEmail(any(SendEmailBaseRequest.class))).thenReturn(new NotificationServiceResponse());

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            UsersDto user = Mockito.mock(UsersDto.class);
            userContext.when(UserContext::getUser).thenReturn(user);

            // Call the sendNotification method
            verifiedGrossMassService.sendNotification(vgm);

            // Verify that email sending occurred
            verify(notificationService, times(1)).sendEmail(any(SendEmailBaseRequest.class));
        }
    }

    @Test
    void testSendNotification_NoEmailTemplateFound() throws JsonProcessingException {
        VerifiedGrossMass vgm = createMockVGM();

        // Mock fetchEmailTemplate to return an empty list (no templates found)
        when(carrierBookingInttraUtil.fetchEmailTemplate(anyList())).thenReturn(Collections.emptyList());

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            UsersDto user = Mockito.mock(UsersDto.class);
            userContext.when(UserContext::getUser).thenReturn(user);

            // Call the sendNotification method
            verifiedGrossMassService.sendNotification(vgm);

            // Verify that email sending did not occur
            verify(notificationService, times(0)).sendEmail(any(SendEmailBaseRequest.class));
        }
    }

    @Test
    void testSendNotification_ExceptionDuringEmailSending() throws JsonProcessingException {
        VerifiedGrossMass vgm = createMockVGM();  // Mock VGM object

        // Mock email template response
        EmailTemplatesRequest emailTemplate = new EmailTemplatesRequest();
        emailTemplate.setType(VERIFIED_GROSS_MASS_EMAIL_TEMPLATE);
        emailTemplate.setSubject("Test Subject");
        emailTemplate.setBody("Test Body");

        // Mock fetchEmailTemplate to return the email template
        when(carrierBookingInttraUtil.fetchEmailTemplate(anyList())).thenReturn(List.of(emailTemplate));
        when(verifiedGrossMassUtil.getSendEmailBaseRequest(vgm)).thenReturn(new ArrayList<>());

        // Mock the email sending to throw an exception
        doThrow(new RuntimeException("Email sending failed")).when(notificationService).sendEmail(any(SendEmailBaseRequest.class));

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            UsersDto user = Mockito.mock(UsersDto.class);
            userContext.when(UserContext::getUser).thenReturn(user);

            // Call the sendNotification method
            verifiedGrossMassService.sendNotification(vgm);

            // Verify that an error was logged
            verify(notificationService, times(1)).sendEmail(any(SendEmailBaseRequest.class));
        }
    }

    @Test
    void testSendNotification_NullEmailTemplate() throws JsonProcessingException {
        VerifiedGrossMass vgm = createMockVGM();  // Mock VGM object

        // Mock fetchEmailTemplate to return a null template
        when(carrierBookingInttraUtil.fetchEmailTemplate(anyList())).thenReturn(Collections.singletonList(null));

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            UsersDto user = Mockito.mock(UsersDto.class);
            userContext.when(UserContext::getUser).thenReturn(user);

            // Call the sendNotification method
            verifiedGrossMassService.sendNotification(vgm);

            // Verify that email sending did not occur
            verify(notificationService, times(0)).sendEmail(any(SendEmailBaseRequest.class));
        }
    }

    @Test
    void updateVgmStatus_shouldUpdateContainerStatus_andSave_whenStatusIsAccepted() {
        CommonContainers container = new CommonContainers();
        container.setContainerNo("CONT123");

        VerifiedGrossMass vgmEntity = new VerifiedGrossMass();
        vgmEntity.setId(10L);
        vgmEntity.setContainersList(List.of(container));

        VgmEventDto event = new VgmEventDto();
        event.setBookingNumber("BOOK123");
        event.setContainerNumber("CONT123");
        event.setStatus("Accepted");

        when(verifiedGrossMassDao.findByCarrierBookingNo("BOOK123")).thenReturn(vgmEntity);

        verifiedGrossMassService.updateVgmStatus(event);

        assertEquals(VerifiedGrossMassStatus.AcceptedByINTTRA.name(), container.getVgmStatus());
        verify(commonContainersRepository).save(container);
        verify(transactionHistoryDao, never()).save(any());
    }

    @Test
    void updateVgmStatus_shouldSaveTransactionHistory_whenStatusIsRejected() {
        CommonContainers container = new CommonContainers();
        container.setContainerNo("CONT123");

        VerifiedGrossMass vgmEntity = new VerifiedGrossMass();
        vgmEntity.setId(10L);
        vgmEntity.setContainersList(List.of(container));

        VgmEventDto event = new VgmEventDto();
        event.setBookingNumber("BOOK123");
        event.setContainerNumber("CONT123");
        event.setStatus("Rejected");

        com.dpw.runner.shipment.services.kafka.dto.inttra.Error error1 = new com.dpw.runner.shipment.services.kafka.dto.inttra.Error();
        error1.setErrorDetails("Missing weight");
        com.dpw.runner.shipment.services.kafka.dto.inttra.Error error2 = new com.dpw.runner.shipment.services.kafka.dto.inttra.Error();
        error2.setErrorDetails("Invalid unit");

        event.setErrors(List.of(error1, error2));

        when(verifiedGrossMassDao.findByCarrierBookingNo("BOOK123")).thenReturn(vgmEntity);

        verifiedGrossMassService.updateVgmStatus(event);

        assertEquals(VerifiedGrossMassStatus.RejectedByINTTRA.name(), container.getVgmStatus());
        verify(commonContainersRepository).save(container);
    }

    @Test
    void updateVgmStatus_shouldDoNothing_whenContainerNumberDoesNotMatch() {
        CommonContainers container = new CommonContainers();
        container.setContainerNo("CONT123");

        VerifiedGrossMass vgmEntity = new VerifiedGrossMass();
        vgmEntity.setId(10L);
        vgmEntity.setContainersList(List.of(container));

        VgmEventDto event = new VgmEventDto();
        event.setBookingNumber("BOOK123");
        event.setContainerNumber("DIFF999");
        event.setStatus("Accepted");

        when(verifiedGrossMassDao.findByCarrierBookingNo("BOOK123")).thenReturn(vgmEntity);

        verifiedGrossMassService.updateVgmStatus(event);

        assertNull(container.getVgmStatus());
        verify(commonContainersRepository, never()).save(any());
        verify(transactionHistoryDao, never()).save(any());
    }

    // ----------- generateErrorMessage() Tests -----------

    @Test
    void generateErrorMessage_shouldReturnEmptyString_whenErrorsIsNullOrEmpty() {
        assertEquals(Constants.EMPTY_STRING, verifiedGrossMassService.generateErrorMessage(null));
        assertEquals(Constants.EMPTY_STRING, verifiedGrossMassService.generateErrorMessage(Collections.emptyList()));
    }

    @Test
    void generateErrorMessage_shouldJoinErrorMessages_withSeparator() {
        com.dpw.runner.shipment.services.kafka.dto.inttra.Error e1 = new com.dpw.runner.shipment.services.kafka.dto.inttra.Error();
        e1.setErrorDetails("Error A");
        com.dpw.runner.shipment.services.kafka.dto.inttra.Error e2 = new com.dpw.runner.shipment.services.kafka.dto.inttra.Error();
        e2.setErrorDetails("Error B");

        String result = verifiedGrossMassService.generateErrorMessage(List.of(e1, e2));

        assertEquals("Error A| Error B", result);
    }

    // ----------- parseIntraStatus() Tests -----------

    @Test
    void parseIntraStatus_shouldReturnCorrectEnum_forAllKnownValues() {
        assertEquals(VerifiedGrossMassStatus.ConditionallyAccepted,
                callParse("ConditionallyAccepted"));
        assertEquals(VerifiedGrossMassStatus.AcceptedByINTTRA,
                callParse("Accepted"));
        assertEquals(VerifiedGrossMassStatus.RejectedByINTTRA,
                callParse("Rejected"));
        assertEquals(VerifiedGrossMassStatus.ReplacedByCarrier,
                callParse("Replaced"));
        assertEquals(VerifiedGrossMassStatus.Changed,
                callParse("Changed"));
        assertEquals(VerifiedGrossMassStatus.Acknowledged,
                callParse("Acknowledged"));
        assertEquals(VerifiedGrossMassStatus.PendingFromCarrier,
                callParse("SomethingElse"));
    }

    // helper to invoke private static method parseIntraStatus
    private VerifiedGrossMassStatus callParse(String status) {
        return Arrays.stream(VerifiedGrossMassService.class.getDeclaredMethods())
                .filter(m -> m.getName().equals("parseIntraStatus"))
                .findFirst()
                .map(m -> {
                    m.setAccessible(true);
                    try {
                        return (VerifiedGrossMassStatus) m.invoke(null, status);
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                })
                .orElseThrow();
    }
}
