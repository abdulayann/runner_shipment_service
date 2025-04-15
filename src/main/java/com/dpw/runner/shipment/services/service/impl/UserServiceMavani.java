package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.service.interfaces.IUserService;
import org.springframework.stereotype.Service;

@Service
public class UserServiceMavani implements IUserService {

    @Override
    public UsersDto getUserByToken(String token) {
        return new UsersDto();
    } // TODO- actually fetch from mavani
}
