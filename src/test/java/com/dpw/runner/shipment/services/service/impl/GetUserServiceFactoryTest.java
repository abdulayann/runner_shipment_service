package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.service.interfaces.IUserService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class GetUserServiceFactoryTest {
    @Mock
    private UserServiceV1 userServiceV1;

    @InjectMocks
    private GetUserServiceFactory getUserServiceFactory;

    @Mock
    UserServiceMavani userServiceMavani;

    @BeforeEach
    void setUp() {
        ReflectionTestUtils.setField(getUserServiceFactory, "source", "");
    }

    @Test
    void testReturnUserService() {
        IUserService result = getUserServiceFactory.returnUserService();
        assertEquals(userServiceV1, result);
    }

    @Test
    void testGetCarrierMasterDataService2() {
        ReflectionTestUtils.setField(getUserServiceFactory, "source", "v1");
        IUserService result = getUserServiceFactory.returnUserService();
        assertEquals(userServiceV1, result);
    }

    @Test
    void testGetCarrierMasterDataService3() {
        ReflectionTestUtils.setField(getUserServiceFactory, "source", "mavani");
        IUserService result = getUserServiceFactory.returnUserService();
        assertEquals(userServiceMavani, result);
    }

    @Test
    void testGetCarrierMasterDataService4() {
        ReflectionTestUtils.setField(getUserServiceFactory, "source", "v2");
        IUserService result = getUserServiceFactory.returnUserService();
        assertEquals(userServiceV1, result);
    }
}
