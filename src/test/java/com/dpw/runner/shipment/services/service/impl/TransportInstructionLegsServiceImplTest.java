package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.enums.TILegType;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.ITiLegDao;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.v1.response.RAKCDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsResponse;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.TiLegs;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IPickupDeliveryDetailsRepository;
import com.dpw.runner.shipment.services.repository.interfaces.ITiLegRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.v3.TransportInstructionValidationUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.lang.reflect.InvocationTargetException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@Execution(CONCURRENT)
class TransportInstructionLegsServiceImplTest {
    @MockBean
    private DependentServiceHelper dependentServiceHelper;

    @MockBean
    private IAuditLogService iAuditLogService;

    @MockBean
    private ITiLegDao iTiLegDao;

    @MockBean
    private ITiLegRepository iTiLegRepository;

    @MockBean
    private JsonHelper jsonHelper;

    @MockBean
    private UserContext userContext;

    @MockBean
    private IPickupDeliveryDetailsRepository pickupDeliveryDetailsRepository;

    @InjectMocks
    private TransportInstructionLegsServiceImpl transportInstructionLegsService;
    @Mock
    private MasterDataKeyUtils masterDataKeyUtils;
    @Mock
    private MasterDataUtils masterDataUtils;
    @MockBean
    ExecutorService executorServiceMasterData;
    @MockBean
    private TransportInstructionValidationUtil transportInstructionValidationUtil;
    @MockBean
    private IV1Service v1Service;
    @MockBean
    private CommonUtils commonUtils;

    @BeforeEach
    void setup() {
        transportInstructionLegsService.executorServiceMasterData = Executors.newFixedThreadPool(2);
    }

    @Test
    void testCreate() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        TiLegs tiLegs = new TiLegs();
        tiLegs.setPickupDeliveryDetailsId(1l);
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);

        PartiesRequest destination = new PartiesRequest();
        Map<String, Object> destinationAddress = new HashMap<>();
        destinationAddress.put("country", "ind");
        destination.setAddressCode("42 Main St");
        destination.setAddressData(destinationAddress);
        destination.setAddressId("42 Main St");
        destination.setCountryCode("GB");
        destination.setEntityId(1L);
        destination.setEntityType("Entity Type");
        destination.setGuid(UUID.randomUUID());
        destination.setId(1L);
        destination.setIsAddressFreeText(true);
        destination.setOrgCode("Org Code");
        destination.setOrgData(new HashMap<>());
        destination.setOrgId("42");
        destination.setTenantId(1);
        destination.setType("Type");

        Map<String, Object> originAddress = new HashMap<>();
        originAddress.put("country", "ind");
        PartiesRequest origin = new PartiesRequest();
        origin.setAddressCode("42 Main St");
        origin.setAddressData(originAddress);
        origin.setAddressId("42 Main St");
        origin.setCountryCode("GB");
        origin.setEntityId(1L);
        origin.setEntityType("Entity Type");
        origin.setGuid(UUID.randomUUID());
        origin.setId(1L);
        origin.setIsAddressFreeText(true);
        origin.setOrgCode("Org Code");
        origin.setOrgData(new HashMap<>());
        origin.setOrgId("42");
        origin.setTenantId(1);
        origin.setType("Type");

        TransportInstructionLegsRequest request = new TransportInstructionLegsRequest();
        request.setLegType(TILegType.EMPTY);
        request.setTiId(1l);
        request.setRemarks("remarks");
        request.setSequence(1L);
        request.setOrigin(origin);
        request.setDestination(destination);
        request.setEstimatedPickup(LocalDateTime.now());
        request.setActualPickup(LocalDateTime.now());
        request.setActualDelivery(LocalDateTime.now().plusDays(3));
        request.setEstimatedDelivery(LocalDateTime.now().plusDays(3));
        PickupDeliveryDetails pickupDeliveryDetails = new PickupDeliveryDetails();
        pickupDeliveryDetails.setTiLegsList(List.of(tiLegs));
        when(iTiLegDao.save(any())).thenReturn(tiLegs);
        when(jsonHelper.convertValue(any(), eq(TiLegs.class))).thenReturn(tiLegs);
        when(jsonHelper.convertValue(any(), eq(TransportInstructionLegsResponse.class))).thenReturn(new TransportInstructionLegsResponse());
        when(pickupDeliveryDetailsRepository.findById(anyLong())).thenReturn(Optional.of(pickupDeliveryDetails));
        TransportInstructionLegsResponse response = transportInstructionLegsService.create(request);
        assertNotNull(response);
    }

    @Test
    void testUpdate() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        TiLegs tiLegs = new TiLegs();
        tiLegs.setPickupDeliveryDetailsId(1l);
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        TransportInstructionLegsRequest request = new TransportInstructionLegsRequest();
        request.setLegType(TILegType.EMPTY);
        request.setId(1l);
        request.setTiId(1l);
        request.setRemarks("remarks");
        request.setSequence(1L);
        when(iTiLegDao.save(any())).thenReturn(tiLegs);
        when(iTiLegDao.findById(anyLong())).thenReturn(Optional.of(tiLegs));
        when(jsonHelper.convertValue(any(), eq(TiLegs.class))).thenReturn(tiLegs);
        when(jsonHelper.convertValue(any(), eq(TransportInstructionLegsResponse.class))).thenReturn(new TransportInstructionLegsResponse());
        when(pickupDeliveryDetailsRepository.findById(anyLong())).thenReturn(Optional.of(new PickupDeliveryDetails()));
        TransportInstructionLegsResponse response = transportInstructionLegsService.update(request);
        assertNotNull(response);
    }

    @Test
    void testDelete3() throws RunnerException, JsonProcessingException, IllegalAccessException, NoSuchFieldException,
            NoSuchMethodException, InvocationTargetException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        TiLegs tiLegs = new TiLegs();
        tiLegs.setId(1L);
        tiLegs.setPickupDeliveryDetailsId(1L);

        TiLegs tiLegs1 = new TiLegs();
        tiLegs1.setId(2L);
        tiLegs1.setPickupDeliveryDetailsId(1L);
        List<TiLegs> tiLegsList = new ArrayList<>();
        tiLegsList.add(tiLegs);
        tiLegsList.add(tiLegs1);
        PickupDeliveryDetails pickupDeliveryDetails = new PickupDeliveryDetails();
        pickupDeliveryDetails.setId(1L);
        pickupDeliveryDetails.setTiLegsList(tiLegsList);
        when(pickupDeliveryDetailsRepository.findById(any())).thenReturn(Optional.of(pickupDeliveryDetails));
        when(iTiLegRepository.findById(any())).thenReturn(Optional.of(tiLegs));
        when(iTiLegDao.findById(Mockito.<Long>any())).thenReturn(Optional.of(tiLegs));
        doNothing().when(iTiLegDao).delete(any());
        transportInstructionLegsService.delete(1L);
        verify(iTiLegDao).findById(Mockito.<Long>any());
    }

    @Test
    void testRetrieveById() {
        TransportInstructionLegsResponse response = new TransportInstructionLegsResponse();
        response.setDropMode("dropmode");
        response.setLegType(TILegType.EMPTY.name());
        response.setRemarks("remarks");
        when(iTiLegDao.findById(anyLong())).thenReturn(Optional.of(new TiLegs()));
        when(jsonHelper.convertValue(any(), eq(TransportInstructionLegsResponse.class))).thenReturn(response);
        TransportInstructionLegsResponse legsResponse = transportInstructionLegsService.retrieveById(1l);
        assertNotNull(legsResponse);
    }
    @Test
    void testRetrieveByIdIn() {
        TransportInstructionLegsResponse response = new TransportInstructionLegsResponse();
        response.setDropMode("dropmode");
        response.setLegType(TILegType.EMPTY.name());
        response.setRemarks("remarks");
        when(iTiLegDao.findByIdIn(Set.of(anyLong()))).thenReturn(List.of(new TiLegs()));
        when(jsonHelper.convertValue(any(), eq(TransportInstructionLegsResponse.class))).thenReturn(response);
        List<TiLegs> legsResponse = transportInstructionLegsService.retrieveByIdIn(Set.of(1l));
        assertNotNull(legsResponse);
    }
    @Test
    void testFindByTransportInstructionId() {
        TransportInstructionLegsResponse response = new TransportInstructionLegsResponse();
        response.setDropMode("dropmode");
        response.setLegType(TILegType.EMPTY.name());
        response.setRemarks("remarks");
        when(iTiLegDao.findByPickupDeliveryDetailsId(anyLong())).thenReturn(List.of(new TiLegs()));
        when(jsonHelper.convertValue(any(), eq(TransportInstructionLegsResponse.class))).thenReturn(response);
        List<TiLegs> legsResponse = transportInstructionLegsService.findByTransportInstructionId(1l);
        assertNotNull(legsResponse);
    }

    @Test
    void testList() {
        List<RAKCDetailsResponse> rakcDetailsResponses = new ArrayList<>();
        rakcDetailsResponses.add(RAKCDetailsResponse.builder().id(10L).build());
        PartiesResponse partiesResponse = PartiesResponse.builder().orgId("1L").addressId("10").build();
        Map<String, RAKCDetailsResponse> rakcDetailsResponseMap = new HashMap<>();
        rakcDetailsResponseMap.put("test", RAKCDetailsResponse.builder().id(10L).build());
        TransportInstructionLegsResponse response = new TransportInstructionLegsResponse();
        response.setOrigin(partiesResponse);
        response.setDestination(partiesResponse);
        when(commonUtils.getRAKCDetailsMap(anyList())).thenReturn(rakcDetailsResponseMap);
        when(v1Service.addressList(any())).thenReturn(V1DataResponse.builder().entities(new Object()).build());
        when(jsonHelper.convertValueToList(any(), eq(RAKCDetailsResponse.class))).thenReturn(rakcDetailsResponses);
        when(jsonHelper.convertValue(Mockito.<TiLegs>any(),
                Mockito.<Class<TransportInstructionLegsResponse>>any())).thenReturn(response);
        TiLegs tiLegs = new TiLegs();
        tiLegs.setDropMode("dropMode");
        tiLegs.setRemarks("remarks");
        ArrayList<TiLegs> content = new ArrayList<>();
        content.add(tiLegs);
        PageImpl<TiLegs> pageImpl = new PageImpl<>(content);
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });
        when(iTiLegDao.findAll(Mockito.<Specification<TiLegs>>any(), Mockito.<Pageable>any()))
                .thenReturn(pageImpl);
        ListCommonRequest listCommonRequest= new ListCommonRequest();
        listCommonRequest.setPopulateRAKC(true);
        TransportInstructionLegsListResponse actualListResult = transportInstructionLegsService
                .list(listCommonRequest, true);
        verify(iTiLegDao).findAll(Mockito.<Specification<TiLegs>>any(), Mockito.<Pageable>any());
        verify(jsonHelper).convertValue(Mockito.<TiLegs>any(),
                Mockito.<Class<TransportInstructionLegsResponse>>any());
        assertEquals(1, actualListResult.getTotalPages().intValue());
        assertEquals(1, actualListResult.getTiLegsResponses().size());
        assertEquals(1L, actualListResult.getTotalCount().longValue());
    }
    @Test
    void testCreate_Exception() {
        TransportInstructionLegsRequest request = new TransportInstructionLegsRequest();
        request.setTiId(1L);
        when(pickupDeliveryDetailsRepository.findById(anyLong())).thenReturn(Optional.empty());
        assertThrows(ValidationException.class, () -> transportInstructionLegsService.create(request));
    }
    @Test
    void testDelete_Exception() {
        when(iTiLegDao.findById(anyLong())).thenReturn(Optional.empty());
        assertThrows(ValidationException.class, () -> transportInstructionLegsService.delete(1L));
    }
}
