package com.dpw.runner.shipment.services.utility;

import com.nimbusds.jwt.JWT;
import com.nimbusds.jwt.JWTClaimsSet;
import com.nimbusds.jwt.JWTParser;
import com.nimbusds.jwt.proc.BadJWTException;
import com.nimbusds.jwt.proc.DefaultJWTClaimsVerifier;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.servlet.http.HttpServletResponse;
import java.text.ParseException;

@Component
@Slf4j
public class TokenUtility {
    private static final String BEARER = "Bearer";
    public static final String NAME_FIELD = "nameid";

    public String getUserNameFromToken(String token, HttpServletResponse res) throws ParseException, BadJWTException {
        String[] tokenSplits = token.split(" ");
        if(tokenSplits.length>2 || !BEARER.equals(tokenSplits[0])) throw new BadJWTException("Expected 'Bearer token'");
        JWT parse = JWTParser.parse(tokenSplits[tokenSplits.length-1]);

        JWTClaimsSet claimsSet = parse.getJWTClaimsSet();
        validateValidity(claimsSet);
        return claimsSet.getSubject();
    }

    private void validateValidity(JWTClaimsSet claimsSet) throws BadJWTException {
        DefaultJWTClaimsVerifier defaultJWTClaimsVerifier=new DefaultJWTClaimsVerifier(claimsSet,null);
        defaultJWTClaimsVerifier.verify(claimsSet,null);
    }

}
